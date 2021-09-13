
import sys
import socket
import string
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
    return [1, -1, current_time, 2, -2]


# Main function
if __name__ == "__main__":
  with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
      s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
      s.bind(('127.0.0.1', 1240))
      s.listen()
      while True:
          conn, addr = s.accept()
          message = ""
          while True:
              data = conn.recv(1024).decode('utf-8')
              if data == "terminate": quit ()
              message += data
              if not data or "%" in data:
                  break
          message = message.split("%")[0]

          t, h, o, b, v, a = parse_input (message)
          motion = action(t, h, o, b, v, a)
          output = format_output(motion)

          conn.sendall(output.encode('utf-8'))
          conn.close()
