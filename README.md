# Parallel Functional 2048
*Colin Brown and Jonathan Rich*
2048-playing agent implemented using parallel adversarial search with alpha-beta pruning

## Usage
./pf2048 int - Play a game of 2048
./pf2048 seq - Have the agent play a game using sequential adversarial search
./pf2048 full - Have the agent play a game using full parallel tree search without alpha-beta pruning
./pf2048 mixed - Have the agent play a game using a Young Brothers Can Wait parallel search strategy
./pf2048 random - Have the agent play a game using a random exploration strategy


## Todos
- [x] Basic IO w/ command line arguments and errors, interactive
- [x] Clean up interface, make into a module
- [ ] Sequential minimax
- [ ] Benchmark
- [ ] Full parallel
- [ ] Mixed parallel w/ YBCW, maybe PV
- [ ] Speedups
- [ ] random exploration of trees to get better alpha-beta values
- [ ] intelligent opponent

### Speedups
- [ ] bitvectors & switch to exponents
- [ ] state heuristic lookup (intmap)
- [ ] move ordering by lookup on current level