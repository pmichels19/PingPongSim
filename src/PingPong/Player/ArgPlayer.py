
import sys
import math


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
    return [ math.sin(current_time)
           , math.sin(last_hit_time)
           , math.cos(current_time)
           , math.cos(last_hit_time)
           ]

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
    return "time 15 point 0.2 0.3 vector 0.4 0.5"


# Main function
if __name__ == "__main__":
    message = sys.argv[1]
    command = message.split("\n")[0]

    if command == "action"   : output = handle_action    (message)
    if command == "collision": output = handle_collision (message)

    print (output)


