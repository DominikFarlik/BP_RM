from enum import Enum

class Operator(Enum):
    NOT = "¬"
    AND = "∧"
    OR = "∨"
    IMPLICATION = "⟹"
    EQUIVALENCE = "↔"


class Clause:
    def __init__(self, parent=None):
        self.clause = []
        self.parent = self if parent is None else parent

    def __str__(self):
        return str(self.clause)

    def append_empty_clause(self, parent):
        self.clause.append(Clause(parent))
        return self.clause[-1]

    def set_clause(self, clause):
        self.clause = clause

    def add_literal(self, literal):
        self.clause.append(literal)

    def add_literals(self, clause):
        for literal in clause:
            self.clause.append(literal)

    def get_parent(self):
        return self.parent

    def get_operator(self):
        for clause in self.clause:
            if clause == Operator.OR.value or clause == Operator.AND.value:
                return clause

    def get_literals(self):
        """Returns list of all literals in clause"""
        literals = []
        for index, clause in enumerate(self.clause):
            if clause.isalpha() and self.clause[index - 1] == Operator.NOT.value:
                literals.append(Operator.NOT.value + clause)
            elif clause.isalpha():
                literals.append(clause)
        return literals

    def remove_implications(self):
        """Removes all implication from formula"""
        clauses = self.clause.copy()
        for index, clause in enumerate(clauses):
            if isinstance(clause, Clause):
                clause.remove_implications()
            elif clause == Operator.IMPLICATION.value:
                self.clause[index] = Operator.OR.value
                self.clause.insert(index - 1, Operator.NOT.value)

    def remove_clause_negations(self):
        """Removes negations in front of clauses"""
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
        """Negates self clause"""
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
        """Removes double negations from all clauses"""
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.remove_clause_negations()
            if clause == Operator.NOT.value and self.clause[index + 1] == Operator.NOT.value:
                del self.clause[index:index + 2]

    def distribute(self):
        """Distributes clauses"""
        for index, clause in enumerate(self.clause):
            if clause == Operator.OR.value:
                first_lit = []
                second_lit = []
                in_operator = clause
                out_operator = Operator
                if isinstance(self.clause[index - 1], Clause):
                    first_lit = self.clause[index - 1].get_literals()
                    out_operator = self.clause[index - 1].get_operator()
                else:
                    pass
                if isinstance(self.clause[index + 1], Clause):
                    second_lit = self.clause[index + 1].get_literals()
                new_clause_list = []
                for i, lit1 in enumerate(first_lit):
                    for lit2 in second_lit:
                        new_clause = Clause()
                        new_clause.add_literal(lit1)
                        new_clause.add_literal(in_operator)
                        new_clause.add_literal(lit2)
                        new_clause_list.append(new_clause)
                        new_clause_list.append(out_operator)
                self.set_clause(new_clause_list[0:len(new_clause_list) - 1])

    def resolute(self):
        clauses = [clause for clause in self.clause if clause != "∧"]
        for index, clause in enumerate(clauses):
            literals = clause.get_literals()
            for i, clause2 in enumerate(clauses[index + 1:len(clauses)]):
                literals2 = clause2.get_literals()
                [print(c, end="") for c in clauses]
                print("\n")
                for l1, literal in enumerate(literals):
                    for l2, literal2 in enumerate(literals2):
                        new_clause = Clause()
                        canResolve = False
                        if len(literal) == 2 and len(literal2) == 1:
                            if literal[1] == literal2:
                                canResolve = True
                        elif len(literal) == 1 and len(literal2) == 2:
                            if literal == literal2[1]:
                                canResolve = True
                        if canResolve:
                            new_lits = literals[0:l1] + literals[l1 + 1:]
                            new_lits2 = literals2[0:l2] + literals2[l2 + 1:]
                            new_clause.add_literals(new_lits + new_lits2)
                            clauses.pop(index)
                            clauses.pop(i - 1)
                            clauses.append(new_clause)
        self.set_clause(clauses)


def parse_formula(input_formula):
    """Parse formula into a list of clauses"""
    formula = Clause()
    current_clause = formula
    for char in input_formula:
        if char == "(":
            current_clause = current_clause.append_empty_clause(current_clause)
        elif char == ")":
            current_clause = current_clause.get_parent()
        else:
            current_clause.add_literal(char) #(A∧¬B)∧(¬C∧¬D∧E)∨((¬A∧B)∨(F∧¬G)∨(F∧H))
    return formula


def print_formula(formula):
    """Return formula in readable format"""
    out_formula = ""
    for clause in formula.clause:
        if isinstance(clause, Clause):
            out_formula += "(" + str(print_formula(clause)) + ")"
        else:
            out_formula += clause
    return out_formula
