from calendar import firstweekday
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

    def get_operators(self):
        operators = []
        for clause in self.clause:
            if clause == Operator.OR.value or clause == Operator.AND.value:
                operators.append(clause)
        return operators

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
                new_clause1.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.IMPLICATION.value,
                                        copy.deepcopy(self.clause[index + 1])])
                new_clause2.set_clause([copy.deepcopy(self.clause[index + 1]), Operator.IMPLICATION.value,
                                        copy.deepcopy(self.clause[index - 1])])
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
        for i in range(2):
            for index, clause in enumerate(self.clause):
                canDistribute = False
                if isinstance(clause, Clause):
                    clause.distribute()
                if clause == Operator.OR.value:
                    first_clause = Clause()
                    second_clause = Clause()
                    if isinstance(self.clause[index - 1], Clause):
                        if isinstance(self.clause[index - 1].clause[0], Clause) and self.clause[index - 1].clause[1] == Operator.AND.value and isinstance(self.clause[index - 1].clause[2], Clause):
                            first_clause.set_clause([self.clause[index - 1].clause[0], Operator.OR.value, copy.deepcopy(self.clause[index + 1])])
                            second_clause.set_clause([self.clause[index - 1].clause[2], Operator.OR.value, copy.deepcopy(self.clause[index + 1])])
                            canDistribute = True
                        elif isinstance(self.clause[index - 1].clause[0], str) and Operator.AND.value in self.clause[index - 1].clause:
                            and_pos = self.clause[index - 1].clause.index(Operator.AND.value)
                            first_clause.set_clause([''.join(self.clause[index - 1].clause[0:and_pos]), Operator.OR.value, copy.deepcopy(self.clause[index + 1])])
                            second_clause.set_clause([''.join(self.clause[index - 1].clause[and_pos + 1: len(self.clause[index - 1].clause)]), Operator.OR.value, copy.deepcopy(self.clause[index + 1])])
                            canDistribute = True
                    elif isinstance(self.clause[index + 1], Clause):
                        if isinstance(self.clause[index + 1].clause[0], Clause):
                            if isinstance(self.clause[index - 1], str):
                                first_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, self.clause[index + 1].clause[0]])
                                second_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, self.clause[index + 1].clause[2]])
                                canDistribute = True
                        elif isinstance(self.clause[index + 1].clause[0], str):
                            if self.clause[index + 1].clause[0] == Operator.NOT.value:
                                first_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, ''.join(self.clause[index + 1].clause[0:2])])
                                if self.clause[index + 1].clause[3] == Operator.NOT.value:
                                    second_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, ''.join(self.clause[index + 1].clause[3:5])])
                                else:
                                    second_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, self.clause[index + 1].clause[3]])
                            else:
                                first_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, self.clause[index + 1].clause[0]])
                                if self.clause[index + 1].clause[2] == Operator.NOT.value:
                                    second_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, ''.join(self.clause[index + 1].clause[2:4])])
                                else:
                                    second_clause.set_clause([copy.deepcopy(self.clause[index - 1]), Operator.OR.value, self.clause[index + 1].clause[2]])
                            canDistribute = True
                    if canDistribute:
                        self.clause[index - 1] = first_clause
                        self.clause[index] = Operator.AND.value
                        self.clause[index + 1] = second_clause
                        print(print_formula(self))


    def connect_clauses_with_same_operators(self):
        for main_clause in self.clause:
            if isinstance(main_clause, Clause):
                main_clause.connect_clauses_with_same_operators()
            operators = []
            for clause in self.clause:
                if isinstance(clause, Clause):
                    operators.extend(clause.get_operators())
                if clause == Operator.AND.value or clause == Operator.OR.value:
                    operators.append(clause)

            if len(set(operators)) == 1:
                new_clause = []
                for clause in self.clause:
                    if isinstance(clause, Clause):
                        for clause2 in clause.clause:
                            new_clause.append(clause2)
                    else:
                        new_clause.append(clause)
                self.clause = new_clause

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
