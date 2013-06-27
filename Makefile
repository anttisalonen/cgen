default:
	cgen -o c --header=Animal.h --header=Dog.h \
		    --header=Sheep.h cpp/Dog.h cpp/Sheep.h cpp/Animal.h
	g++ -c -Icpp -o c/Animal.o c/Animal.cpp
	g++ -c -Icpp -o c/Dog.o c/Dog.cpp
	g++ -c -Icpp -o c/Sheep.o c/Sheep.cpp
	ar q libanimals.a c/Animal.o c/Dog.o c/Sheep.o
	grgen -o graph cpp/Animal.h cpp/Dog.h cpp/Sheep.h
	cgen-hs -o hs --inherit=graph c/Animal.h c/Sheep.h c/Dog.h
	cp Main.hs hs
	cd hs && ghc --make -o Main -L.. -lanimals -lstdc++ Main.hs
	hs/Main

clean:
	rm -rf hs c graph libanimals.a
