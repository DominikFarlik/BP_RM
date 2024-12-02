from enum import Enum

from Formula import remove_double_negations


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

    def set_clause(self, clause):
        self.clause = clause

    def add_literal(self, literal):
        self.clause.append(literal)

    def get_parent(self):
        return self.parent

    def get_clause_operator(self):
        for clause in self.clause:
            if clause == Operator.OR.value or clause == Operator.AND.value:
                return clause

    def get_literals(self):
        literals = []
        for index, clause in enumerate(self.clause):
            if clause.isalpha() and self.clause[index - 1] == Operator.NOT.value:
                literals.append(Operator.NOT.value + clause)
            elif clause.isalpha():
                literals.append(clause)
        return literals

    def remove_implications(self):
        clauses = self.clause.copy()
        for index, clause in enumerate(clauses):
            if isinstance(clause, Clause):
                clause.remove_implications()
            elif clause == Operator.IMPLICATION.value:
                self.clause[index] = Operator.OR.value
                self.clause.insert(index - 1, Operator.NOT.value)

    def remove_clause_negations(self):
        clauses = self.clause.copy()
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.remove_clause_negations()
            elif clause == Operator.NOT.value and isinstance(clauses[index + 1], Clause):
                self.clause.pop(index)
                self.clause[index].negate_self()
            if clause == Operator.NOT.value and clauses[index + 1] == Operator.NOT.value:
                del self.clause[index:index + 2]
        self.remove_double_negations()

    def negate_self(self):
        negated_clause = []
        for clause in self.clause:
            if isinstance(clause, Clause):
                clause.negate_self()
                negated_clause.append(clause)
            elif clause == Operator.OR.value:
                negated_clause.append(Operator.AND.value)
            elif clause == Operator.AND.value:
                negated_clause.append(Operator.OR.value)
            elif clause == Operator.NOT.value:
                negated_clause.append(Operator.NOT.value)
            else:
                negated_clause.append(Operator.NOT.value)
                negated_clause.append(clause)
        self.clause = negated_clause

    def remove_double_negations(self):
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.remove_clause_negations()
            if clause == Operator.NOT.value and self.clause[index + 1] == Operator.NOT.value:
                del self.clause[index:index + 2]

    def distribute_disjunctions(self):
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.distribute_disjunctions()
            elif clause == Operator.OR.value or clause == Operator.AND.value:
                if isinstance(self.clause[index - 1], str) and isinstance(self.clause[index + 1], Clause):
                    literal_operator = clause
                    clause_operator = self.clause[index + 1].get_clause_operator()
                    first_clause_literal = "".join(self.clause[index - 2:index]) if self.clause[index - 2] == Operator.NOT.value else self.clause[index - 1]
                    second_clause_literals = self.clause[index + 1].get_literals()
                    clause1 = Clause(self)
                    clause1.set_clause(first_clause_literal + literal_operator + second_clause_literals[0])
                    clause2 = Clause(self)
                    clause2.set_clause(first_clause_literal + literal_operator + second_clause_literals[1])
                    distributed_clause = [clause1, clause_operator, clause2]
                    self.clause = distributed_clause


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
