# Chord P2P system
Project 3 source code of DOSP(Distributed Operating System Principle)
Develop by Wei Zihan (Alex)

# This project implemented part of the chord algorithm descriped in this paper:
https://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf
which includes: find_successor(), join(), stabilize(), notify(), fix_finger().
After the chord is spawn, each node will send request to each other, and the average hops will be printed

# This project is developed using InteliJ IDEA, I used Erlang gen_server package. 
Before run the code, configure your IDEA with erlang settings, and in "build", use erlang console.
