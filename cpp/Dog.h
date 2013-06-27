/* cpp/Dog.h */
#ifndef ANIMALS_DOG_H
#define ANIMALS_DOG_H

#include "Animal.h"

namespace Animals {
	class Dog : public Animal {
		public:
			Dog() { }
			void make_sound()
			{
				std::cout << "Growl!\n";
			}
	};
}

#endif
