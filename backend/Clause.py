from enum import Enum


class Operator(Enum):
    NOT = "¬"
    AND = "∧"
    OR = "∨"
    IMPLICATION = "→"
    EQUIVALENCE = "↔"


class Clause:
    def __init__(self, parent=None):
        self.clause = []
        self.parent = self if parent is None else parent

    def __str__(self):
        return str(self.clause)

    def add_clause(self, parent):
        self.clause.append(Clause(parent))
        return self.clause[-1]

    def add_literal(self, literal):
        self.clause.append(literal)

    def get_parent(self):
        return self.parent

    def remove_implication(self):
        clauses = self.clause.copy()
        for index, clause in enumerate(clauses):
            if isinstance(clause, Clause):
                clause.remove_implication()
            elif clause == Operator.IMPLICATION.value:
                self.clause[index] = Operator.OR.value
                self.clause.insert(index - 1, Operator.NOT.value)


def parse_formula(input_formula):
    formula = Clause()
    current_clause = formula
    for char in input_formula:
        if char == "(":
            current_clause = current_clause.add_clause(current_clause)
        elif char == ")":
            current_clause = current_clause.get_parent()
        else:
            current_clause.add_literal(char)
    return formula


def print_formula(formula):
    out_formula = ""
    for clause in formula.clause:
        if isinstance(clause, Clause):
            out_formula += "(" + str(print_formula(clause)) + ")"
        else:
            out_formula += clause
    return out_formula
