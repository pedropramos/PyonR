#lang python

'Help! Help! I\'m being repressed!'

fifth_letter = "MONTY"[4]

parrot = "Norwegian Blue"
print len(parrot)
print parrot.lower()
print parrot.upper()

pi = 3.14
print str(pi)

print "Spam " + "and " + "eggs"
print "The value of pi is around " + str(3.14)

string_1 = "Camelot"
string_2 = "place"
print "Let's not go to %s. 'Tis a silly %s." % (string_1, string_2)

name = raw_input("What is your name?")
quest = raw_input("What is your quest?")
color = raw_input("What is your favorite color?")
print "Ah, so your name is %s, your quest is %s, and your favorite color is %s." % (name, quest, color)