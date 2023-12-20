GHC = ghc
FLAGS = --make
SRC_DIR = src
TEST_DIR = tests
MAIN = $(TEST_DIR)/SimplexTest.hs
EXECUTABLE = SimplexTest

.PHONY: all clean test

all: test

test:
	$(GHC) $(FLAGS) -i$(SRC_DIR) $(MAIN) -o $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE)
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.hi
	rm -f $(TEST_DIR)/*.o $(TEST_DIR)/*.hi
