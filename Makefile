PWD = `pwd`

all:
	ln -s $(PWD) ${HOME}/.xmonad

clean:
	rm ${HOME}/.xmonad
