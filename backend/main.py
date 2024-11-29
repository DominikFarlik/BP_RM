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

            if isinstance(clauses[0], str):
                if len(clauses[0]) == 1 and clauses[0] in Operator:
                    # print("Operator: " + str(clauses[0]))
                    pass

                elif len(clauses[0]) == 1 and clauses[0].isalpha:
                    # print("Literal: " + str(clauses[0]))
                    pass

                elif len(clauses[0]) == 2 and clauses[0][0] == Operator.NOT.value:
                    # print("Negated literal: " + str(clauses[0]))
                    pass

                elif len(clauses[0]) > 1:
                    # print("Clause: " + str(clauses[0]))
                    pass

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
                if len(clauses[0]) == 1 and clauses[0] in Operator:
                    pass

                elif len(clauses[0]) == 1 and clauses[0].isalpha:
                    pass

                elif len(clauses[0]) == 2 and clauses[0][0] == Operator.NOT.value:
                    pass

                elif len(clauses[0]) > 1:
                    pass


if __name__ == "__main__":
    input_formula = "(¬(P∨Q)→R)∧¬A∧(¬R∨S)∧(¬(W∧A)∨B∨(A∨D))"
    print("User input:\n" + input_formula + "\n")
    formula1 = Formula(input_formula)
    print("First split cycle:\n" + str(formula1.clauses) + "\n")
    formula1.split_to_clauses_2nd_cycle()
    print("Second split cycle:\n" + str(formula1.clauses) + "\n")
    formula1.remove_implications_and_equivalences()  # TODO: finish  func
    print("Removed implications:\n" + str(formula1.clauses) + "\n")
    formula1.convert_to_NNF()
    print("Converted to NNF:\n" + str(formula1.clauses) + "\n")