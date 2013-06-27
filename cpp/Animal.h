/* cpp/Animal.h */
#ifndef ANIMALS_ANIMAL_H
#define ANIMALS_ANIMAL_H

#include <iostream>

namespace Animals {
	class Animal {
		public:
			Animal()
				: age(0) { }
			~Animal() { }
			virtual void make_sound() = 0;
			int get_age() const
			{
				return age;
			}
			void increment_age()
			{
				age++;
			}
		private:
			int age;
	};
}

#endif
