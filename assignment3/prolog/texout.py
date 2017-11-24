import os
with open("trial.txt") as file:
    for line in file:
        line = line.strip()
        if line[0]=="P":
            print(line)
        if line[0] == "G":
            print(line)
            print()

