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
            if not x:
                pass
            elif isinstance(x[0], str):
                if len(x[0]) > 2:
                    formula += "(" + x[0] + ")"
                elif len(x[0]) == 2:
                    formula += x[0]
                elif len(x[0]) == 1:
                    formula += x[0]

            elif isinstance(x[0], list):
                formula += "("
                for y in x[0]:
                    if len(y[0]) > 2:
                        formula += "(" + y[0] + ")"
                    elif len(y[0]) == 1:
                        formula += y[0]
                    elif len(y[0]) == 2:
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

    def remove_implications_and_equivalences(self):  # TODO: Add equivalence remove
        """Remove implication from formula"""
        for x, clauses in enumerate(self.clauses):
            if isinstance(clauses[0], list):
                for y, clause in enumerate(clauses[0]):
                    if Operator.IMPLICATION.value in clause[0]:
                        if len(clause[0]) == 1:  # Implication on 2nd layer between 2 lists
                            self.clauses[x][0] = remove_implication_between_lists(clauses[0])
                        else:  # Implication in 2nd layer bracket
                            self.clauses[x][0][y][0] = remove_implication_in_string(clause[0])

            elif isinstance(clauses[0], str) and Operator.IMPLICATION.value in clauses[0]:
                if len(clauses[0]) > 2:  # Implication in 1st layer bracket
                    self.clauses[x] = [remove_implication_in_string(clauses[0])]

                elif len(clauses[0]) == 1:  # Implication on 1st layer between 2 lists
                    self.clauses[x][0] = Operator.OR.value
                    self.clauses.insert(x - 1, [Operator.NOT.value])

    def convert_to_NNF(self):
        """Use De Morgan's laws to remove negations"""
        for x, clauses in enumerate(self.clauses):
            if clauses[0] == Operator.NOT.value:  # Negation on 1st layer before bracket
                self.clauses[x + 1] = [negate_clause(self.clauses[x + 1][0])]
                remove_double_negations(self.clauses[x + 1][0])
                self.clauses.pop(x)

    def disjunction_distribution(self):
        """Remove disjunctions from formula to achieve CNF"""
        remove_redundant_brackets(self.clauses)
        distribute_layer2_brackets(self.clauses)

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

def remove_implication_in_string(clause):
    new_clause = ""
    for i, literal in enumerate(clause):
        if literal == Operator.IMPLICATION.value:
            if i > 2 and clause[i - 2] == Operator.NOT.value:
                new_clause += clause[i - 1]
            else:
                new_clause += Operator.NOT.value + clause[i - 1]

            new_clause += Operator.OR.value

            if i < len(clause[0]) - 1 and clause[i + 1] == Operator.NOT.value:
                new_clause += Operator.NOT.value + clause[i + 2]
            else:
                new_clause += clause[i + 1]
    return new_clause


def remove_implication_between_lists(clause):
    new_clause = []
    for i, literal in enumerate(clause):
        literal = literal[0]
        if literal == Operator.IMPLICATION.value:
            if i > 2 and clause[i - 2] == [Operator.NOT.value]:
                new_clause.append(clause[i - 1])

            elif clause[i - 1][0][0] == Operator.NOT.value and len(clause[i - 1][0]) == 2:
                new_clause.append([clause[i - 1][0][1]])

            else:
                new_clause.append([Operator.NOT.value])
                new_clause.append(clause[i - 1])

            new_clause.append([Operator.OR.value])
            new_clause.append(clause[i + 1])

    return new_clause


def negate_literal(literal):
    if literal.isalpha():
        return Operator.NOT.value + literal

    elif literal == Operator.OR.value:
        return Operator.AND.value

    elif literal == Operator.AND.value:
        return Operator.OR.value

    elif literal == Operator.NOT.value:
        return Operator.NOT.value


def negate_clause(clause):
    new_clauses = []
    for literal in clause:
        literal = literal[0]
        if len(literal) > 2:
            l2_clause = ""
            for literal2 in literal:
                l2_clause += negate_literal(literal2[0])
            new_clauses.append([l2_clause])
        else:
            new_clauses.append([negate_literal(literal)])

    return new_clauses


def remove_double_negations(clause):
    for i, literal in enumerate(clause):
        if "¬¬" in literal[0]:
            double_neg = literal[0].find("¬¬")
            clause[i] = [clause[i][0][:double_neg] + clause[i][0][double_neg + 2:]]


def remove_redundant_brackets(formula):
    for i, clauses in enumerate(formula):
        clauses = clauses[0]
        if isinstance(clauses, list):
            operator = ""
            isRedundant = True
            for clause in clauses:
                clause = clause[0]
                for literal in clause:
                    if literal == Operator.OR.value or literal == Operator.AND.value:
                        if operator == "":
                            operator = literal
                        elif operator != literal:
                            isRedundant = False
                            break
            if isRedundant:
                new_clause = ""
                for clause in clauses:
                    new_clause += clause[0]
                formula[i] = [new_clause]


def distribute_layer2_brackets(formula):
    for i, clauses in enumerate(formula):
        clauses = clauses[0]
        if isinstance(clauses, list):  # find layer2 brackets
            if len(formula[i][0]) == 3:  #  find 2 literals with one operand
                new_clause = []
                lit_operator = formula[i][0][1][0]
                if len(formula[i][0][0]) <= 2:
                    lit1 = formula[i][0][0][0]
                    clause_operator = formula[i][0][2][0][1]
                    new_clause.append([lit1 + lit_operator + formula[i][0][2][0][0]])
                    new_clause.append([clause_operator])
                    new_clause.append([lit1 + lit_operator + formula[i][0][2][0][2]])
                else:
                    pass  # TODO
                formula[i] = [new_clause]