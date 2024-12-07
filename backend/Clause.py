from enum import Enum
import copy


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

    def remove_equivalences(self):
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.remove_equivalences()
            elif clause == Operator.EQUIVALENCE.value and isinstance(self.clause[index - 1], Clause):
                new_clause1 = Clause(self)
                new_clause2 = Clause(self)
                new_clause1.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.IMPLICATION.value, copy.deepcopy(self.clause[index + 1])])
                new_clause2.set_clause([copy.deepcopy(self.clause[index + 1]), Operator.IMPLICATION.value, copy.deepcopy(self.clause[index - 1])])
                self.clause = [new_clause1, Operator.AND.value, new_clause2]

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
                clause.remove_double_negations()
            if clause == Operator.NOT.value and self.clause[index + 1] == Operator.NOT.value:
                del self.clause[index:index + 2]

    def distribute(self):
        """Distributes clauses"""
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                if len(clause.clause) == 3:
                    new_clause = Clause(self)
                    left_clause = Clause(new_clause)
                    right_clause = Clause(new_clause)
                    left_first = Clause(left_clause)
                    right_first = Clause(right_clause)

                    if isinstance(clause.clause[0].clause[0], Clause):
                         left_first.set_clause([clause.clause[0].clause[0]])
                         right_first.set_clause([clause.clause[0].clause[2]])

                    elif clause.clause[0].clause[0] == Operator.NOT.value:
                        left_first = clause.clause[0].clause[1]
                        if clause.clause[0].clause[3] == Operator.NOT.value:
                            right_first = clause.clause[0].clause[3] + clause.clause[0].clause[4]
                        else:
                            right_first = clause.clause[3]
                    else:
                        left_first = clause.clause[0].clause[0]
                        if clause.clause[0].clause[2] == Operator.NOT.value:
                            right_first = clause.clause[0].clause[2] + clause.clause[0].clause[3]
                        else:
                            right_first = clause.clause[2]

                    left_clause.set_clause([left_first, Operator.OR.value, copy.deepcopy(clause.clause[2])])
                    right_clause.set_clause([right_first, Operator.OR.value, copy.deepcopy(clause.clause[2])])
                    new_clause.set_clause([left_clause, Operator.AND.value, right_clause])
                    print(print_formula(new_clause))

    def resolute(self):
        clauses = self.get_list_of_clauses()
        finished = False
        while not finished:
            print("New resolvent: " + str(clauses[-1]) + "\nCurrent clauses: " + str(clauses))
            canResolve = False
            for c1, clause in enumerate(clauses):
                if canResolve:
                    break
                for l1, literal in enumerate(clause):
                    if canResolve:
                        break
                    for c2, clause2 in enumerate(clauses):
                        if c1 == c2 or canResolve:
                            break
                        for l2, literal2 in enumerate(clause2):
                            if len(literal) == 2:
                                if literal[1] == literal2:
                                    canResolve = True
                            elif len(literal2) == 2:
                                if literal2[1] == literal:
                                    canResolve = True

                            if canResolve:
                                clauses.append(clause[0:l1] + clause[l1 + 1:len(clause)] +
                                               clause2[0:l2] + clause2[l2 + 1:len(clause2)])
                                clauses.remove(clause)
                                clauses.remove(clause2)
                                break
            if not canResolve:
                if len(clauses[0]) == 2:
                    if len(clauses[0][0]) == 2:
                        if clauses[0][0][1] == clauses[0][1]:
                            clauses.pop(0)
                    elif len(clauses[0][1]) == 2:
                        if clauses[0][1][1] == clauses[0][0]:
                            clauses.pop(0)
                finished = True
        return clauses

    def get_list_of_clauses(self):
        clauses = []
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clauses.append(clause.get_literals())
        return clauses


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
            current_clause.add_literal(char)
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
