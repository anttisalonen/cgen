/* cpp/Sheep.h */
#ifndef ANIMALS_SHEEP_H
#define ANIMALS_SHEEP_H

#include "Animal.h"

namespace Animals {
	class Sheep : public Animal {
		public:
			Sheep(int wooliness_level_ = 0)
				: wooliness_level(wooliness_level_)
			{ }
			void make_sound()
			{
				std::cout << "Baa!\n";
			}
			void shear()
			{ /* something */ }
		private:
			int wooliness_level;
	};
}

#endif
