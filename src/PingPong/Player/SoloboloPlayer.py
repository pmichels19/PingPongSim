
import sys
import socket
import string
import math


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


### COLLISION

# Handle a "collision" message
def handle_collision(message):
    t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2 = parse_collision_input (message)
    return collision (t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2)

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

    t1 = (-b + math.sqrt(d)) / 2 * a
    t2 = (-b - math.sqrt(d)) / 2 * a
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

### MAIN

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

          conn.sendall(output.encode('utf-8'))
          conn.close()
