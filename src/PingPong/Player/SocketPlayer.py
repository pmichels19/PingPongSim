
import sys
import socket
import string
import math


# The port you will use to communicate.
# Change this to something unique! Otherwise, if your opponent also uses sockets
# and uses the same port, weird things will happen.
port = 6174

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
    return [1.1, 0.8, 0.5, 0.2, 2]


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
def collision(t1, xp1, yp1, xq1, yq1, xr1, yr1, t2, xp2, yp2, xq2, yq2, xr2, yr2):
    return "time 15 point 0.1 0.2 vector 0.1 0.2"


### PLAN


# Handle a "plan" message
def handle_plan(message):
    foot, arm, xp, yp, xn, yn, xv, yv = parse_plan_input (message)
    return plan (foot, arm, xp, yp, xn, yn, xv, yv)

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
    return "impossible"


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
          if command == "plan"     : output = handle_plan      (message)

          conn.sendall(output.encode('utf-8'))
          conn.close()
