
import sys
import socket
import string
import math
import numpy as np


# The port you will use to communicate.
# Change this to something unique! Otherwise, if your opponent also uses sockets
# and uses the same port, weird things will happen.
port = 1251

### ACTION

# Handle a "action" message
def handle_action(message):
    t, h, o, b, v, a = parse_action_input (message)
    motion = action(t, h, o, b, v, a)
    return format_action_output(motion)

# Takes a formatted string and breaks it into seperate variables
def parse_action_input(data): 
    lines = data.split("\n")
    line1_words = lines[1].split(" ")
    line2_words = lines[2].split(" ")
    line3_words = lines[3].split(" ")
    line4_words = lines[4].split(" ")
    current_time    = float(line1_words[0])
    last_hit_time   = float(line2_words[0])
    last_hit_object = line2_words[1] + " " + line2_words[2]
    ball_location   = (float(line3_words[1]), float(line3_words[2]))
    ball_velocity   = (float(line4_words[1]), float(line4_words[2]))
    arm_configuration = lines[5]
    return current_time, last_hit_time, last_hit_object, ball_location, ball_velocity, arm_configuration

# Takes a list of joint speeds and formats it.as a string
def format_action_output(motion):
    string_motion = [str(value) for value in motion]
    formatted_motion = " ".join(string_motion)
    return formatted_motion

# The function where the magic happens
def action(current_time, last_hit_time, last_hit_object, ball_location, ball_velocity, arm_configuration):
    # get the angles of all joints
    arm = arm_configuration.split(" ")
    joint_angles = []
    for i in range( len(arm) ):
        if arm[i] == "joint":
            i = i + 1
            joint_angles.append( float( arm[i] ) )
    return [math.sin(current_time), math.cos(current_time), math.sin(current_time), joint_angles[-1] - (joint_angles[0] + joint_angles[1] + joint_angles[2])]

############################################################################################################################################################################
############################################################################################################################################################################
###############################################################################  COLLISION  ################################################################################
############################################################################################################################################################################
############################################################################################################################################################################

# Handle a "collision" message
def handle_collision(message):
    t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2 = parse_collision_input (message)
    # error handling to be sure
    try:
        return collision (t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2)
    except:
        return "no collision"

# Takes a formatted string and breaks it into seperate variables
def parse_collision_input(data):
    lines = data.split("\n")
    line1_words = lines[1].split(" ")
    line2_words = lines[2].split(" ")
    t1  = float(line1_words[1])
    xp1 = float(line1_words[3])
    yp1 = float(line1_words[4])
    xq1 = float(line1_words[6])
    yq1 = float(line1_words[7])
    xr1 = float(line1_words[8])
    yr1 = float(line1_words[9])
    t2  = float(line2_words[1])
    xp2 = float(line2_words[3])
    yp2 = float(line2_words[4])
    xq2 = float(line2_words[6])
    yq2 = float(line2_words[7])
    xr2 = float(line2_words[8])
    yr2 = float(line2_words[9])
    return t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2

# The collision function
def collision(t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2, reverse = False):
    # get time of impact
    toi = getCollisionTime(xp1, yp1, xq1, yq1, xr1, yr1, xp2, yp2, xq2, yq2, xr2, yr2)
    if toi == -1:
        if not reverse:
            return collision(t1, xp1, yp1, xr1, yr1, xq1, yq1, t2, xp2, yp2, xr2, yr2, xq2, yq2, True)
        return "no collision"
    
    # check if p is on the line segment rq at time toi
    xpt, ypt, xqt, yqt, xrt, yrt = getPointsAtTOI(toi, xp1, yp1, xq1, yq1, xr1, yr1, xp2, yp2, xq2, yq2, xr2, yr2)
    on, mu = onLineSegment(xpt, ypt, xqt, yqt, xrt, yrt)
    if not on:
        if not reverse:
            return collision(t1, xp1, yp1, xr1, yr1, xq1, yq1, t2, xp2, yp2, xr2, yr2, xq2, yq2, True)
        return "no collision"
    
    # get actual time of impact from [0, 1] toi
    t = t1 + toi * (t2 - t1)

    # calculate velocity of ball after collision
    vx, vy = getVelocity(mu, xp2, yp2, xq2, yq2, xr2, yr2, xpt, ypt, xqt, yqt, xrt, yrt, t2 - t, t2 - t1)

    return f"time {t} point {xpt} {ypt} vector {vx} {vy}"

def getVelocity(mu, xp2, yp2, xq2, yq2, xr2, yr2, xpt, ypt, xqt, yqt, xrt, yrt, tremaining, ttotal):
    # get velocity of ball after collision
    vpx, vpy = getVelocityPoint(xpt, ypt, xp2, yp2)
    vpx = vpx / tremaining
    vpy = vpy / tremaining
    # get velocity of point on line segment, let's call it s
    # last two parameters here are wrong
    vsx, vsy = getVelocityPoint(xpt, ypt, xr2 + mu * (xq2 - xr2), yr2 + mu * (yq2 - yr2))
    vsx = vsx / tremaining
    vsy = vsy / tremaining
    # find relative velocity of p w.r.t. line segment
    vrelx = vpx - vsx
    vrely = vpy - vsy
    # get slope of line segment
    dy = yqt - yrt
    dx = xqt - xrt
    # dx of 0 is equivalent to reflecting in y-axis
    if dx != 0:
        m = dy / dx
        ce = 1 / (1 + (m ** 2))
        vreturnx = (ce * ((vrelx * (1 - (m ** 2))) + (2 * vrely * m))) + vsx
        vreturny = (ce * ((2 * vrelx * m) + (vrely * ((m ** 2) - 1)))) + vsy
    else:
        vreturnx = -vrelx + vsx
        vreturny = vrely + vsy
    
    # vector from pt* to pt2
    vt2x = vreturnx * tremaining / ttotal * (ttotal / tremaining)
    vt2y = vreturny * tremaining / ttotal * (ttotal / tremaining)
    # return result
    return (vt2x, vt2y)

# get the velocity of one point given its start and endpoint during the motion
def getVelocityPoint(x1, y1, x2, y2):
    return (x2 - x1, y2 - y1)

def onLineSegment(xpt, ypt, xqt, yqt, xrt, yrt):
    drp = distance(xrt, yrt, xpt, ypt)
    dpq = distance(xpt, ypt, xqt, yqt)
    drq = distance(xrt, yrt, xqt, yqt)
    return abs(drq - (drp + dpq)) < 1e-8, drp / drq

def distance(ax, ay, bx, by):
    return math.sqrt(((bx - ax) ** 2) + ((by - ay) ** 2))

def getPointsAtTOI(toi, xp1, yp1, xq1, yq1, xr1, yr1, xp2, yp2, xq2, yq2, xr2, yr2):
    xpt = (1 - toi) * xp1 + toi * xp2
    ypt = (1 - toi) * yp1 + toi * yp2
    xqt = (1 - toi) * xq1 + toi * xq2
    yqt = (1 - toi) * yq1 + toi * yq2
    xrt = (1 - toi) * xr1 + toi * xr2
    yrt = (1 - toi) * yr1 + toi * yr2
    return (xpt, ypt, xqt, yqt, xrt, yrt)

# get time of collision, if this returns -1, then there is no collision
def getCollisionTime(xp1, yp1, xq1, yq1, xr1, yr1, xp2, yp2, xq2, yq2, xr2, yr2):
    # calculate abc formula values
    a = - xq1 * yr1 + xp1 * yr1 + xq2 * yr1 - yr1 * xp2 + xq1 * yp1 - xr1 * yp1 + xq1 * yr2 + xr1 * yq1 - yq1 * xp1 - yr2 * xp1 - yp1 * xq2 - yr2 * xq2 + yp1 * xr2 - yq1 * xr2 + xp1 * yq2 - xr1 * yq2 + xr2 * yq2 + xq2 * yp2 - xq1 * yp2 - xr2 * yp2 + xr1 * yp2 - yq2 * xp2 + yq1 * xp2 + yr2 * xp2
    b = 2 * xq1 * yr1 - 2 * xp1 * yr1 - xq2 * yr1 + yr1 * xp2 - 2 * xq1 * yp1 + 2 * xr1 * yp1 - xq1 * yr2 - 2 * xr1 * yq1 + 2 * yq1 * xp1 + yr2 * xp1 + yp1 * xq2 - yp1 * xr2 + yq1 * xr2 - xp1 * yq2 + xr1 * yq2 + xq1 * yp2 - xr1 * yp2 - yq1 * xp2
    c = - xq1 * yr1 + yr1 * xp1 + xq1 * yp1 - xr1 * yp1 + xr1 * yq1 - yq1 * xp1
    d = (b * b) - 4 * a * c
    
    # if a is neglibible then t goes to - c / b
    if abs(a) < 1e-8:
        # if b is also 0 we have no solutions
        if abs(b) < 1e-8:
            return -1
        t = - c / b
        return getEarliest(t, -1)

    # if d < 0 there are no colissions, so we can return 
    if d < 0:
        return -1

    t1 = (-b + math.sqrt(d)) / (2 * a)
    t2 = (-b - math.sqrt(d)) / (2 * a)
    return getEarliest(t1, t2)

# get the lowest of t1 and t2 in [0, 1], if none are in [0, 1], return -1
def getEarliest(t1, t2):
    if outRange(t1) and outRange(t2):
        return -1
    if outRange(t1):
        return t2
    if outRange(t2):
        return t1
    if t1 < t2:
        return t1
    return t2

# check if t is ouside of the range [0, 1]
def outRange(t):
    return t < 1e-8 or t > (1 - 1e-8)

############################################################################################################################################################################
############################################################################################################################################################################
################################################################################  PLANNING  ################################################################################
############################################################################################################################################################################
############################################################################################################################################################################

# Handle a "plan" message
def handle_plan(message):
    foot, arm, xp, yp, xn, yn, xv, yv = parse_plan_input (message)
    # some error handling just to be sure
    try:
        return plan (foot, arm, xp, yp, xn, yn, xv, yv)
    except:
        return "impossible"

# Takes a formatted string and breaks it into seperate variables
def parse_plan_input(data):
    lines = data.split("\n")
    line1_words = lines[1].split(" ")
    line2_words = lines[2].split(" ")
    foot = float(line1_words[1])
    del line1_words [0:2]
    arm  = line1_words
    xp   = float(line2_words[1])
    yp   = float(line2_words[2])
    xn   = float(line2_words[4])
    yn   = float(line2_words[5])
    xv   = float(line2_words[7])
    yv   = float(line2_words[8])
    return foot, arm, xp, yp, xn, yn, xv, yv

# The plan function
def plan(foot, arm, xp, yp, xn, yn, xv, yv):
    # get rid of the word 'arm' in the first entry of the arm
    del arm[0]
    # extract current arm configuration to a more sensible list of lists: [joint, rotation] or [link, length]
    i = 0
    arm_config = []
    while i in range(len(arm)):
        arm_config.append( [arm[i], float(arm[i + 1])] )
        i = i + 2
    # get end points of target line segment using normal vector and middle point
    xr, yr, xq, yq = getBatEndpoints(xp, yp, xn, yn)
    # use endpoints as target base and end for bat in both directions to be sure
    target_angles = getTargetAngles(foot, arm_config, xr, yr, xq, yq, 400)
    rotateToTarget(arm_config, target_angles)
    # check if the angles approximate the desired segment well enough
    if not isClose(foot, arm_config, xr, yr, xq, yq):
        target_angles = getTargetAngles(foot, arm_config, xq, yq, xr, yr, 400)
        rotateToTarget(arm_config, target_angles)
        # if both are not close enough it is impossibleto approximate
        if not isClose(foot, arm_config, xq, yq, xr, yr):
            return "impossible"
    # get the speeds needed to achieve the velocity provided
    speeds = getSpeeds(arm_config, xv, yv)
    # parse the angles and speeds to right format for returning
    result_angles = parseAngles(target_angles)
    result_speeds = parseSpeeds(speeds)
    return f"{result_angles}\n{result_speeds}"

def getSpeeds(arm, xv, yv):
    jacobian = getJacobian(arm)
    jacobian = np.array([jacobian[0], jacobian[1]])
    jointCount = 0
    for tup in arm:
        if tup[0] == "joint":
            jointCount += 1
    # if the determinant is effectively 0, use the psuedo-inverse
    inverted = np.zeros((jointCount, jointCount))
    try:
        inverted = np.linalg.inv(jacobian)
    except:
        try:
            inverted = np.linalg.pinv( jacobian )
        except:
            # if both of the inverse methods fails, just return 0 speeds and cry :(
            return np.zeros(jointCount).tolist()
    speeds = np.matmul(inverted, np.array([xv, yv]))
    return speeds.tolist()

def getJacobian(arm):
    # gather the columns of the jacobian matrix in a list
    columns = []
    jointNr = 0
    # get the upper three values of the last column of T(0, end)
    toEndMatrix = getDHMatrixToJoint(0, arm, 0)
    toEndEffector = np.array([toEndMatrix[0, -1], toEndMatrix[1, -1], 0])
    for tup in arm:
        if tup[0] == "link":
            continue
        jointNr += 1
        rotation = getDHRotationMatrix(arm, jointNr)
        # potential FIXME: this might now work with a foot of 0
        transformation = getDHMatrixToJoint(0, arm, jointNr)
        toCurJoint = np.array([transformation[0, -1], transformation[1, -1], 0])
        # get angular velocities for this joint
        angular = np.matmul( rotation, np.array([0, 0, 1]) ) # angular: [w1, w2, w3]
        # use angular velocities to make a cross product with vector to end effector
        diff = np.subtract(toEndEffector, toCurJoint)
        linear = np.cross( np.array([0, 0, 1]), diff )
        # append to columns
        columns.append( np.append( linear, angular ) )
    # make the matrix by transposing the columns
    jacobian = np.array(columns).transpose()
    return jacobian

def parseSpeeds(speeds):
    result = "speeds"
    for speed in speeds:
        result += f" {speed}"
    return result

def parseAngles(angles):
    result = "angles"
    for angle in angles:
        result += f" {angle}"
    return result

def rotateToTarget(arm, angles):
    for i in range(len(angles)):
            jointCount = 0
            for tup in arm:
                if tup[0] != "joint":
                    continue
                if jointCount != i:
                    jointCount += 1
                    continue
                tup[1] = angles[i]

def isClose(foot, arm, xr, yr, xq, yq):
    basex, basey = getEndEffector(foot, arm[:-2])
    endx, endy = getEndEffector(foot, arm)
    return (distance(xr, yr, basex, basey) < 1e-3) and (distance(xq, yq, endx, endy) < 1e-3)

# Coordinate descent, will try to put base of last link on r and end of last link on q => assumes |rq| = |last link|
def getTargetAngles(foot, arm, xr, yr, xq, yq, limit):
    # get the arm without the last link and its joint
    noBatArm = arm[:-2]
    # get the amount of joints in the batless arm
    jointCount = 0
    for tup in noBatArm:
        if tup[0] == "joint":
            jointCount += 1
    # start from the upper most joint
    root = jointCount
    for _ in range(limit):
        if root == 0:
            root += jointCount
        
        # get root coordinates of current link
        rootx, rooty = getSegRoot(foot, noBatArm, root)
        # get coordinates of end effector
        endx, endy = getEndEffector(foot, noBatArm)
        # check if end effector is close enough to desired location
        if distance(endx, endy, xr, yr) < 1e-8:
            break

        # get the angle by which to turn
        turnAngle = getCorrectionAngle(xr, yr, endx, endy, rootx, rooty)
        if np.isnan(turnAngle):
            continue

        # change the angle of the joint in question
        count = 0
        for tup in noBatArm:
            if tup[0] != "joint":
                continue

            count += 1
            if count == root:
                tup[1] += turnAngle
                break
        
        # go down the joints
        root = root - 1
    
    # copy the angles of the arm without bat to the original arm
    for i in range(len(noBatArm)):
        if noBatArm[i][0] == "joint":
            arm[i][1] = noBatArm[i][1]
    # get root coordinates of current link
    rootx, rooty = getSegRoot(foot, arm, jointCount + 1)
    # get coordinates of end effector
    endx, endy = getEndEffector(foot, arm)
    # extract the final angles from the arm
    result_angles = []
    for tup in arm:
        if tup[0] == "joint":
            result_angles.append(tup[1])
    # get the rotation needed to align the bat properly
    lastCorrection = getCorrectionAngle(xq, yq, endx, endy, rootx, rooty)
    if np.isnan(lastCorrection):
        return result_angles
    
    result_angles[-1] += lastCorrection
    return result_angles

# get the angle needed to align (end - root), the current vector, with (xr - root), the target vector
def getCorrectionAngle(targetx, targety, endx, endy, rootx, rooty):
        # normalized vector from current root to current end effector
        rootEndx, rootEndy = normalize(endx - rootx, endy - rooty)
        # normalized vector from current root to desired end location
        rootDesx, rootDesy = normalize(targetx - rootx, targety - rooty)
        # calculate dot product of rootEnd and rootDes to get cos(angle) between the two vectors
        cosAngle = rootEndx * rootDesx + rootEndy * rootDesy
        if abs(1 - cosAngle) < 1e-8:
            return float("nan")
        # check z value of cross product of rootDes x rootEnd to get direction of rotation
        crossz = rootDesx * rootEndy - rootDesy * rootEndx
        # get angle from cosAngle
        turnAngle = math.acos(cosAngle)
        # rotate the current joint
        if crossz > 0:
            turnAngle = turnAngle * -1
        return turnAngle


# gives the normalized vector described by x and y
def normalize(x, y):
    size = math.sqrt((x ** 2) + (y ** 2))
    if size < 1e-8:
        return (0, 0)
    return ((x / size), (y / size))

# get the transformation matrix giving the x and y coordinate of the (joint)'s joint
def getDHMatrixToJoint(foot, arm, joint):
    transformation = np.array([[1, 0, foot], [0, 1, 0], [0, 0, 1]])
    joint_count = 0
    for tup in arm:
        # increase the joint count if we get a joint
        if tup[0] == "joint":
            joint_count += 1
            # if this joint brings us to the joint_base joint, we have found the desired root
            if joint_count == joint:
                break
            phi = tup[1]
            transformation = np.matmul(transformation, np.array([[math.cos( phi ), -math.sin( phi ), 0], [math.sin( phi ), math.cos( phi ), 0], [0, 0, 1]]))
        elif tup[0] == "link":
            transformation = np.matmul(transformation, np.array([[1, 0, 0], [0, 1, tup[1]], [0, 0, 1]]))
    return transformation

# gets the rotation matrix up to joint - 1. So if joint == 1, then you get R(0, 0), joint == 2 gives R(0, 1), joint ==3 gives R(0, 2) etc.
def getDHRotationMatrix(arm, joint):
    rotation = np.identity(3)
    joint_count = 0
    for tup in arm:
        if tup[0] == "link":
            continue
        joint_count += 1
        # if this joint brings us to the joint_base joint, we have found the desired root
        if joint_count == joint:
            break
        phi = tup[1]
        rotation = np.matmul(rotation, np.array([[math.cos( phi ), -math.sin( phi ), 0], [math.sin( phi ), math.cos( phi ), 0], [0, 0, 1]]))
    return rotation

# goes through the arm, until the joint_base joint
def getSegRoot(foot, arm, joint_base):
    transformation = getDHMatrixToJoint(foot, arm, joint_base)
    root = np.matmul(transformation, np.array([[0], [0], [1]]))
    return (root[0, 0], root[1, 0])

# returns the end effector, i.e. the root of the link forming the bat
def getEndEffector(foot, arm):
    return getSegRoot(foot, arm, 0)

def getBatEndpoints(xp, yp, xn, yn):
    xl = -yn
    yl =  xn
    normLen = math.sqrt( (xl ** 2) + (yl ** 2) )
    scaledx = (0.05 / normLen) * xl
    scaledy = (0.05 / normLen) * yl
    xr = xp + scaledx
    yr = yp + scaledy
    xq = xp - scaledx
    yq = yp - scaledy
    return (xr, yr, xq, yq)

############################################################################################################################################################################
############################################################################################################################################################################
##################################################################################  MAIN  ##################################################################################
############################################################################################################################################################################
############################################################################################################################################################################

# Main function
if __name__ == "__main__":
  with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
      s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
      s.bind(('127.0.0.1', port))
      s.listen()
      while True:
          conn, addr = s.accept()
          message = ""
          while True:
              data = conn.recv(port).decode('utf-8')
              if data == "terminate": quit ()
              message += data
              if not data or "%" in data:
                  break
          message = message.split("%")[0]

          command = message.split("\n")[0]

          if command == "action"   : output = handle_action    (message)
          if command == "collision": output = handle_collision (message)
          if command == "plan"     : output = handle_plan      (message)

          conn.sendall(output.encode('utf-8'))
          conn.close()
