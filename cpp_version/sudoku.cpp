#include <iostream>
#include <vector>
#include <set>

using std::set;
using std::vector;
using std::cin;
using std::cout;
using std::endl;

constexpr int SUDOKU_SIZE = 9;

bool row_conflict(vector<vector<int>>& puzzle, int row) {
    set<int> nums;
    for (int i = 0; i < SUDOKU_SIZE; ++i) {
        if (nums.count(puzzle[row][i]) > 0) {
            return true;
        }
        if (puzzle[row][i] != 0) {
            nums.insert(puzzle[row][i]);
        }
    }
    return false;
}

bool col_conflict(vector<vector<int>>& puzzle, int col) {
    set<int> nums;
    for (int i = 0; i < SUDOKU_SIZE; ++i) {
        if (nums.count(puzzle[i][col]) > 0) {
            return true;
        }
        if (puzzle[i][col] != 0) {
            nums.insert(puzzle[i][col]);
        }
    }
    return false;
}

bool box_conflict(vector<vector<int>>& puzzle, int row, int col) {
    set<int> nums;
    constexpr int BOX_SIZE = SUDOKU_SIZE / 3;
    int box_row = row - (row % BOX_SIZE);
    int box_col = col - (col % BOX_SIZE);
    for (int i = box_row; i < box_row + BOX_SIZE; ++i) {
        for (int j = box_col; j < box_col + BOX_SIZE; ++j) {
            if (nums.count(puzzle[i][j]) > 0) {
                return true;
            }
            if (puzzle[i][j] != 0) {
                nums.insert(puzzle[i][j]);
            }
        }
    }
    return false;
}

bool has_conflict(vector<vector<int>>& puzzle, int row, int col) {
    return row_conflict(puzzle, row) || col_conflict(puzzle, col) || box_conflict(puzzle, row, col);
}

void next(int& row, int& col) {
    int temp = (row * SUDOKU_SIZE) + col + 1;
    row = temp / SUDOKU_SIZE;
    col = temp % SUDOKU_SIZE;
}

void prev(int& row, int& col) {
    int temp = (row * SUDOKU_SIZE) + col - 1;
    row = temp / SUDOKU_SIZE;
    col = temp % SUDOKU_SIZE;
}

// returns a bool indicating if the puzzle has a solution
bool solve(vector<vector<int>>& puzzle, int row, int col) {
    if (row == SUDOKU_SIZE) {
        return true;
    }
    if (puzzle[row][col] != 0) {
        next(row, col);
        bool ans = solve(puzzle, row, col);
        prev(row, col);
        return ans;
    }
    for (int i = 1; i <= SUDOKU_SIZE; ++i) {
        puzzle[row][col] = i;
        if (!has_conflict(puzzle, row, col)) {
            next(row, col);
            bool solvable = solve(puzzle, row, col);
            prev(row, col);
            if (solvable) {
                return solvable;
            }
        }
    }
    puzzle[row][col] = 0;
    return false;
}

int main() {
    // read in puzzle
    vector<vector<int>> puzzle = vector<vector<int>>(SUDOKU_SIZE, vector<int>(SUDOKU_SIZE, 0));
    for (int i = 0; i < SUDOKU_SIZE; ++i) {
        for (int j = 0; j < SUDOKU_SIZE; ++j) {
            cin >> puzzle[i][j];
        }
    }
    cout << endl;

    // solve puzzle
    bool solvable = solve(puzzle, 0, 0);
    if (!solvable) {
        cout << "No Solution" << endl;
        return 0;
    }

    // output puzzle
    for (vector<int>& row : puzzle) {
        for (int num : row) {
            cout << num << " ";
        }
        cout << endl;
    }
}