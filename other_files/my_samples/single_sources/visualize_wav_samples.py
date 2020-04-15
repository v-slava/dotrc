#!/usr/bin/python3

import sys
from scipy.io import wavfile
from matplotlib import pyplot

fps, data = wavfile.read('input_file.wav')
num_samples = data.shape[0]
x = [a / fps for a in range(0, num_samples)]
y = [sample[0] for sample in data]
pyplot.plot(x, y)
pyplot.xlabel('time')
pyplot.ylabel('sample[0]')
pyplot.title('samples')
pyplot.show()
