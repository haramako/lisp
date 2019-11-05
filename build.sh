if [ ! -d build/linux ]; then
	cmake -S . -B build/linux
fi

cmake --build build/linux

