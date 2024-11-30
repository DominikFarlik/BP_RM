from enum import Enum


class Operator(Enum):
    NOT = "¬"
    AND = "∧"
    OR = "∨"
    IMPLICATION = "→"
    EQUIVALENCE = "↔"


class Formula:
    def __init__(self, formula):
        self.clauses = self.split_to_clauses_1st_cycle(formula)

    def __str__(self):
        formula = ""
        for x in self.clauses:
            if isinstance(x[0], str):
                if len(x[0]) > 2:
                    formula += "(" + x[0] + ")"
                elif len(x[0]) == 2:
                    formula += x[0]
                elif len(x[0]) == 1:
                    formula += x[0]
            if isinstance(x[0], list):
                formula += "("
                for y in x[0]:
                    if len(y[0]) > 1:
                        formula += "(" + y[0] + ")"
                    elif len(y[0]) == 1:
                        formula += y[0]
                formula += ")"
        print(self.clauses)
        return formula

    @staticmethod
    def get_first_clause_from_bracket(clause, index):
        """Returns clause from first bracket of larger clause"""
        clause_start_pos = index
        open_bracket_count = 0
        end_bracket_count = 0
        while index < len(clause):
            if clause[index] == "(":
                open_bracket_count += 1
                index += 1

            elif clause[index] == ")":
                end_bracket_count += 1
                index += 1

                if open_bracket_count == end_bracket_count:
                    new_clause = [clause[clause_start_pos + 1:index - 1]]
                    return new_clause, index

            else:
                index += 1

    def extract_clauses(self, clause):
        """Searching for new clauses in formula"""
        new_clause = []
        index = 0
        while index < len(clause):
            if clause[index] == "(":
                split_clause, index = self.get_first_clause_from_bracket(clause, index)
                new_clause.append(split_clause)

            elif clause[index].isalpha():
                new_clause.append([clause[index]])
                index += 1

            elif clause[index] in Operator:
                if clause[index] != Operator.NOT.value:
                    new_clause.append([clause[index]])
                    index += 1
                else:
                    if clause[index + 1].isalpha():
                        new_clause.append([clause[index:index + 2]])
                        index += 2
                    else:
                        new_clause.append([clause[index]])
                        index += 1
            else:
                index += 1
        return new_clause

    def split_to_clauses_1st_cycle(self, formula):
        """Splitting clauses to lists by literals and brackets"""
        new_clauses = []
        new_clause = []
        not_changed_clause = []
        clause = formula

        if "(" not in clause:
            not_changed_clause.append(clause)
        else:
            new_clause = self.extract_clauses(clause)

        if new_clause:
            new_clauses = new_clause
        if not_changed_clause:
            new_clauses.append = not_changed_clause
        return new_clauses

    def split_to_clauses_2nd_cycle(self):
        """Splitting clauses in 2 brackets deep clauses"""
        for x, clauses in enumerate(self.clauses):
            not_changed_clause = []
            for y, clause in enumerate(clauses):
                new_clause = []
                if "(" not in clause:
                    not_changed_clause = clause
                else:
                    new_clause = self.extract_clauses(clause)
                if new_clause:
                    self.clauses[x][y] = new_clause
                if not_changed_clause:
                    self.clauses[x][y] = not_changed_clause

    def remove_implications_and_equivalences(self):
        """Remove implication from formula"""
        for x, clauses in enumerate(self.clauses):
            if isinstance(clauses[0], list):
                for y, clause in enumerate(clauses[0]):
                    if Operator.IMPLICATION.value in clause[0]:
                        if len(clause[0]) == 1:
                            self.clauses[x][0][y] = [Operator.OR.value]
                            if self.clauses[x][0][y - 2] == [Operator.NOT.value]:
                                self.clauses[x][0].pop(y - 2)
                            else:
                                self.clauses.insert(x - 2, [Operator.NOT.value])

            elif isinstance(clauses[0], str) and Operator.IMPLICATION.value in clauses[0]:
                if len(clauses[0]) > 2:
                    new_clause = ""
                    for z, lit in enumerate(clauses[0]):
                        if lit == Operator.IMPLICATION.value:
                            if self.clauses[x][0][z - 2] == Operator.NOT.value:
                                new_clause += self.clauses[x][0][z - 1]
                            else:
                                new_clause += Operator.NOT.value + self.clauses[x][0][z - 1]

                            new_clause += Operator.OR.value

                            if self.clauses[x][0][z + 1].isalpha():
                                new_clause += self.clauses[x][0][z + 1]
                            elif self.clauses[x][0][z + 1] == Operator.NOT.value:
                                new_clause += self.clauses[x][0][z + 1:z + 3]

                    self.clauses[x] = [new_clause]

                elif len(clauses[0]) == 1:
                    self.clauses[x][0] = Operator.OR.value
                    self.clauses.insert(x - 1, [Operator.NOT.value])

    def convert_to_NNF(self):
        """Use De Morgan's laws to remove negations"""
        for x, clauses in enumerate(self.clauses):
            if isinstance(clauses[0], list):
                for y, clause in enumerate(clauses[0]):
                    if Operator.NOT.value in clause[0]:
                        if len(clause[0]) == 1:
                            self.clauses[x][0].pop(y)

                            if len(self.clauses[x][0][y][0]):
                                new_lit = ""
                                for lit in self.clauses[x][0][y][0]:
                                    if lit.isalpha():
                                        new_lit += Operator.NOT.value + lit
                                    if lit == Operator.OR.value:
                                        new_lit += Operator.AND.value
                                    if lit == Operator.AND.value:
                                        new_lit += Operator.OR.value
                                self.clauses[x][0][y][0] = new_lit

            if isinstance(clauses[0], str):
                if len(clauses[0]) == 1 and clauses[0] in Operator.NOT.value:
                    self.clauses.pop(x)
                    if len(self.clauses[x][0]) > 1:
                        new_lit = ""
                        for y, lit in enumerate(self.clauses[x][0]):
                            if lit.isalpha():
                                if self.clauses[x][0][y - 1] != Operator.NOT.value:
                                    new_lit += Operator.NOT.value
                                new_lit += lit
                            if lit == Operator.OR.value:
                                new_lit += Operator.AND.value
                            if lit == Operator.AND.value:
                                new_lit += Operator.OR.value
                            if lit == Operator.NOT.value:
                                pass
                        self.clauses[x] = [new_lit]

    def disjunction_distribution(self):
        """Remove disjunctions from formula to achieve CNF"""
        for x, clauses in enumerate(self.clauses):
            if clauses[0] == Operator.OR.value:
                if len(self.clauses[x - 1][0]) > 2 >= len(self.clauses[x + 1][0]):
                    new_clause = []
                    for y, lit in enumerate(self.clauses[x - 1][0]):
                        if lit == Operator.AND.value or lit == Operator.OR.value:
                            if self.clauses[x - 1][0][y - 1].isalpha() and self.clauses[x - 1][0][y - 2] == Operator.NOT.value:
                                new_clause.append([self.clauses[x - 1][0][y - 2:y] + Operator.OR.value + self.clauses[x + 1][0]])
                                new_clause.append([Operator.AND.value])
                                if self.clauses[x - 1][0][y + 2].isalpha() and self.clauses[x - 1][0][y + 1] == Operator.NOT.value:
                                    new_clause.append([self.clauses[x - 1][0][y + 1:y + 3] + Operator.OR.value + self.clauses[x + 1][0]])
                                    self.clauses.pop(x - 1)
                                    self.clauses.pop(x - 1)
                                    self.clauses.pop(x - 1)
                                    self.clauses.insert(x - 1, new_clause[0])  # TODO: SIMPLIFY and make universal
                                    self.clauses.insert(x, new_clause[1])
                                    self.clauses.insert(x + 1, new_clause[2])

                            elif self.clauses[x - 1][0][y - 1].isalpha():
                                pass

    def is_formula_in_cnf(self):
        """Check if formula is in CNF"""
        for clauses in self.clauses:
            if clauses[0] == Operator.OR.value:
                return False
        return True

    def negate_formula_for_resolution(self):
        """Negate formula to decide its feasibility"""
        pass

    def resolute_formula(self):
        """Use resolution method to get conclusion if formula is feasible"""
        for x, clauses in enumerate(self.clauses):
            if clauses[0] != Operator.AND.value:
                print(clauses[0])




if __name__ == "__main__":  # TODO: simplify storing literals as str instead of list
    input_formula = "(A→B)∨(¬B→¬A)"
    print("User input:\n" + input_formula + "\n")
    formula1 = Formula(input_formula)
    print("First split cycle:\n" + str(formula1) + "\n")
    formula1.split_to_clauses_2nd_cycle()
    print("Second split cycle:\n" + str(formula1) + "\n")
    formula1.remove_implications_and_equivalences()  # TODO: finish  func
    print("Removed implications:\n" + str(formula1) + "\n")
    formula1.convert_to_NNF()  # TODO: finish  func
    print("Converted to NNF:\n" + str(formula1) + "\n")
    formula1.disjunction_distribution()
    print("Disjunction distribution:\n" + str(formula1) + "\n")
    if formula1.is_formula_in_cnf():
        formula1.negate_formula_for_resolution()
        print("Negate each clause for resolution:\n" + str(formula1) + "\n")
        print("Resolution")
        formula1.resolute_formula()
