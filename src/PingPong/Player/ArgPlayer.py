
import sys
import math

# Takes a formatted string and breaks it into seperate variables
def parse_input(data): 
    lines = data.split("\n")
    line0_words = lines[0].split(" ")
    line1_words = lines[1].split(" ")
    line2_words = lines[2].split(" ")
    line3_words = lines[3].split(" ")
    current_time    = float(line0_words[0])
    last_hit_time   = float(line1_words[0])
    last_hit_object = line1_words[1] + " " + line1_words[2]
    ball_location   = (float(line2_words[1]), float(line2_words[2]))
    ball_velocity   = (float(line3_words[1]), float(line3_words[2]))
    arm_configuration = lines[4]
    return current_time, last_hit_time, last_hit_object, ball_location, ball_velocity, arm_configuration

# Takes a list of joint speeds and formats it.as a string
def format_output(motion):
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

# Main function
if __name__ == "__main__":
    input = sys.argv[1]
    t, h, o, b, v, a = parse_input (input)
    motion = action(t, h, o, b, v, a)
    output = format_output(motion)
    print (output)
