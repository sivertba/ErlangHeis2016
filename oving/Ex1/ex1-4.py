# Python 3.3.3 and 2.7.6
# python helloworld_python.py

from threading import Thread

i = 0

# Potentially useful thing:
#   In Python you "import" a global variable, instead of "export"ing it when you declare it
#   (This is probably an effort to make you feel bad about typing the word "global")

def foo():
	global i
	for x in xrange(1,100000):
		i+=1

def bar():
	global i
	for x in xrange(1,100000):
		i-=1

def main():
	global i
	first = Thread(target = foo, args = (),)
	first.start()

	second = Thread(target = bar, args = (),)
	second.start()


	first.join()
	second.join()

	print(i)

main()