from enum import Enum
import copy


class Operator(Enum):
    NOT = "¬"
    AND = "∧"
    OR = "∨"
    IMPLICATION = "→"
    EQUIVALENCE = "↔"


def convert_to_set_of_clauses(clauses):
    clause_set = ""
    if not clauses:
        return "{}"
    for lit in clauses:
        if isinstance(lit, list):
            clause_set += "{" + ', '.join(lit) + "} "
        elif isinstance(lit, str):
            clause_set += "{" + ', '.join(clauses) + "} "
            break
    return clause_set


def process_formula(input_formula):
    steps = ["User input: " + str(input_formula)]
    formula = parse_formula(str(input_formula))
    print(formula_to_str(formula))
    formula.remove_equivalences()
    steps.append("Removed equivalences: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.remove_implications()
    steps.append("Removed implications: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.remove_clause_negations()
    steps.append("Removed clause negations: " + formula_to_str(formula))
    print(formula_to_str(formula))

    while True:
        formula_before_distribution = copy.deepcopy(formula)
        formula.distribute()
        formula.connect_clauses_with_same_operators()
        if formula_to_str(formula_before_distribution) == formula_to_str(formula):
            break

    steps.append("Distributed formula: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.connect_clauses_with_same_operators()
    print(formula_to_str(formula))

    resolution, resolution_steps = formula.resolute()
    steps.extend(resolution_steps)
    if resolution:
        print("Final resolvent: " + convert_to_set_of_clauses(resolution) + "Formula is not feasible.")
        result = "Final resolvent: " + convert_to_set_of_clauses(resolution) + "Formula is not feasible."
        return result, steps
    else:
        print("Empty clause left -> Formula is feasible.")
        result = "Empty clause left -> Formula is feasible."
        return result, steps



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

    def add_clause(self, clause: list):
        new_clause = Clause()
        new_clause.set_clause(clause)
        self.clause.append(new_clause)

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

    def get_list_of_clauses(self):
        clauses = []
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clauses.append(clause.get_literals())
            elif clause == Operator.AND.value or clause == Operator.OR.value:
                pass
            elif clause.isalpha() and self.clause[index - 1] == Operator.NOT.value:
                clauses.append(Operator.NOT.value + clause)
            elif clause.isalpha():
                clauses.extend(clause)
        return clauses

    def remove_equivalences(self):
        for index, clause in enumerate(self.clause):
            if isinstance(clause, Clause):
                clause.remove_equivalences()
            elif clause == Operator.EQUIVALENCE.value:
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
                clause.distribute()
            if clause == Operator.OR.value:
                if isinstance(self.clause[0], Clause) and isinstance(self.clause[2], Clause):  # Clause OR Clause
                    if Operator.AND.value in self.clause[0].clause and Operator.AND.value in self.clause[2].clause:  # [lit AND lit] OR [lit AND lit]
                        first_clause_AND_pos = self.clause[0].clause.index(Operator.AND.value)
                        second_clause_AND_pos = self.clause[2].clause.index(Operator.AND.value)
                        new_clause = Clause()
                        first_lit = []
                        second_lit = []
                        third_lit = []
                        fourth_lit = []
                        first_lit.extend(self.clause[0].clause[0:first_clause_AND_pos])
                        first_lit.append(Operator.OR.value)
                        first_lit.extend(self.clause[2].clause[0:second_clause_AND_pos])
                        new_clause.add_clause(first_lit)
                        new_clause.add_literal(Operator.AND.value)
                        second_lit.extend(self.clause[0].clause[0:first_clause_AND_pos])
                        second_lit.append(Operator.OR.value)
                        second_lit.extend(self.clause[2].clause[second_clause_AND_pos + 1:len(self.clause[2].clause)])
                        new_clause.add_clause(second_lit)
                        new_clause.add_literal(Operator.AND.value)
                        third_lit.extend(self.clause[0].clause[first_clause_AND_pos + 1:len(self.clause[2].clause)])
                        third_lit.append(Operator.OR.value)
                        third_lit.extend(self.clause[2].clause[0:second_clause_AND_pos])
                        new_clause.add_clause(third_lit)
                        new_clause.add_literal(Operator.AND.value)
                        fourth_lit.extend(self.clause[0].clause[first_clause_AND_pos + 1:len(self.clause[2].clause)])
                        fourth_lit.append(Operator.OR.value)
                        fourth_lit.extend(self.clause[2].clause[second_clause_AND_pos + 1:len(self.clause[2].clause)])
                        new_clause.add_clause(fourth_lit)
                        self.clause = new_clause.clause

                    # [lit OR lit] OR [lit AND lit]
                    elif Operator.OR.value in self.clause[0].clause and Operator.AND.value in self.clause[2].clause:
                        second_clause_AND_pos = self.clause[2].clause.index(Operator.AND.value)
                        new_clause = Clause()
                        first_lit = []
                        second_lit = []
                        first_lit.extend(self.clause[0].clause)
                        first_lit.append(Operator.OR.value)
                        first_lit.extend(self.clause[2].clause[0:second_clause_AND_pos])
                        new_clause.add_clause(first_lit)
                        new_clause.add_literal(Operator.AND.value)
                        second_lit.extend(self.clause[0].clause)
                        second_lit.append(Operator.OR.value)
                        second_lit.extend(self.clause[2].clause[second_clause_AND_pos + 1:len(self.clause[2].clause)])
                        new_clause.add_clause(second_lit)
                        self.clause = new_clause.clause

                    # [lit AND lit] OR [lit OR lit]
                    elif Operator.AND.value in self.clause[0].clause and Operator.OR.value in self.clause[2].clause:
                        self.clause = [self.clause[2], Operator.OR.value, self.clause[0].clause]
                        self.distribute()

                # lit or lit or lit ... or [Clause]
                elif len(set(self.get_operators())) == 1 and len(self.get_operators()) > 1 and any(isinstance(item, Clause) for item in self.clause):
                    first_clause = []
                    second_clause = []
                    for literal in self.clause:
                        if isinstance(literal, Clause):
                            first_clause.extend(literal.clause[0].clause)
                            second_clause.extend(literal.clause[2].clause)
                        else:
                            first_clause.append(literal)
                            second_clause.append(literal)
                    new_clause = Clause()
                    new_clause.add_clause(first_clause)
                    new_clause.add_literal(Operator.AND.value)
                    new_clause.add_clause(second_clause)
                    self.clause = new_clause.clause

                # Clause OR not lit
                elif isinstance(self.clause[0], Clause) and self.clause[2] == Operator.NOT.value and isinstance(self.clause[3], str):
                    # [lit AND lit] OR not lit
                    if Operator.AND.value in self.clause[0].clause:
                        first_clause_AND_pos = self.clause[0].clause.index(Operator.AND.value)
                        new_clause = Clause()
                        first_lit = []
                        second_lit = []
                        first_lit.extend(self.clause[0].clause[0:first_clause_AND_pos])
                        first_lit.append(Operator.OR.value)
                        first_lit.extend([self.clause[2], self.clause[3]])
                        new_clause.add_clause(first_lit)
                        new_clause.add_literal(Operator.AND.value)
                        second_lit.extend(self.clause[0].clause[first_clause_AND_pos + 1:len(self.clause[0].clause)])
                        second_lit.append(Operator.OR.value)
                        second_lit.extend([self.clause[2], self.clause[3]])
                        new_clause.add_clause(second_lit)
                        self.clause = new_clause.clause

                # Clause OR lit
                elif isinstance(self.clause[0], Clause) and isinstance(self.clause[2], str):
                    # [lit AND lit] OR lit
                    if Operator.AND.value in self.clause[0].clause:
                        first_clause_AND_pos = self.clause[0].clause.index(Operator.AND.value)
                        new_clause = Clause()
                        first_lit = []
                        second_lit = []
                        first_lit.extend(self.clause[0].clause[0:first_clause_AND_pos])
                        first_lit.append(Operator.OR.value)
                        first_lit.extend(self.clause[2])
                        new_clause.add_clause(first_lit)
                        new_clause.add_literal(Operator.AND.value)
                        second_lit.extend(self.clause[0].clause[first_clause_AND_pos + 1:len(self.clause[0].clause)])
                        second_lit.append(Operator.OR.value)
                        second_lit.extend(self.clause[2])
                        new_clause.add_clause(second_lit)
                        self.clause = new_clause.clause

                # lit OR Clause
                elif isinstance(self.clause[0], str) and isinstance(self.clause[2], Clause):
                    self.clause = [self.clause[2], Operator.OR.value, self.clause[0]]
                    self.distribute()
                # not lit OR Clause
                elif self.clause[0] == Operator.NOT.value and isinstance(self.clause[1], str) and isinstance(self.clause[3], Clause):
                    self.clause = [self.clause[3], Operator.OR.value, self.clause[0], self.clause[1]]
                    self.distribute()

    def connect_clauses_with_same_operators(self):
        for index, main_clause in enumerate(self.clause):
            if isinstance(main_clause, Clause):
                main_clause.connect_clauses_with_same_operators()
            if isinstance(main_clause, Clause):
                operators = []
                operators.extend(main_clause.get_operators())
                if index > 0:
                    operators.append(self.clause[index - 1])
                if index < len(self.clause) - 1:
                    operators.append(self.clause[index + 1])
                if len(set(operators)) == 1:
                    new_clause = []
                    new_clause.extend(self.clause[0:index])
                    for clause in main_clause.clause:
                        new_clause.append(clause)
                    new_clause.extend(self.clause[index + 1:len(self.clause)])
                    self.clause = new_clause

    def resolute(self):
        resolution_steps = []
        clauses = self.get_list_of_clauses()
        resolution_steps.append("Set of clauses for resolution: " + convert_to_set_of_clauses(clauses))
        new_resolvent = False
        while True:
            if new_resolvent:
                new_resolvent = False
            for c1 in clauses:
                if new_resolvent:
                    break
                for l1 in c1:
                    if new_resolvent:
                        break
                    for c2 in clauses:
                        if new_resolvent:
                            break
                        for l2 in c2:
                            if len(l1) == 1:
                                if Operator.NOT.value + l1 == l2:
                                    resolvent = self.get_resolvent(c1, c2)
                                    resolution_steps.append("Remaining clauses" + convert_to_set_of_clauses(clauses))
                                    steps = []
                                    steps.extend("From clause: " + convert_to_set_of_clauses(c1))
                                    steps.extend(", and: " + convert_to_set_of_clauses(c2) + ".")
                                    steps.extend(" New resolvent: " + convert_to_set_of_clauses(resolvent))
                                    resolution_steps.append(steps)
                                    clauses.remove(c1)
                                    clauses.remove(c2)
                                    if resolvent:
                                        clauses.append(resolvent)
                                    new_resolvent = True
                                    break
                            if len(l1) == 2:
                                if l1[1] == l2:
                                    resolvent = self.get_resolvent(c1, c2)
                                    resolution_steps.append("Remaining clauses" + convert_to_set_of_clauses(clauses))
                                    steps = []
                                    steps.extend("From clause: " + convert_to_set_of_clauses(c1))
                                    steps.extend(", and: " + convert_to_set_of_clauses(c2) + ".")
                                    steps.extend(" New resolvent: " + convert_to_set_of_clauses(resolvent))
                                    resolution_steps.append(steps)
                                    clauses.remove(c1)
                                    clauses.remove(c2)
                                    if resolvent:
                                        clauses.append(resolvent)
                                    new_resolvent = True
                                    break
            if not new_resolvent:
                break
        resolution_steps.append("Result: " + convert_to_set_of_clauses(clauses))
        return clauses, resolution_steps

    @staticmethod
    def get_resolvent(c1, c2):
        same_literals = []
        for l1 in c1:
            if len(l1) == 1:
                if Operator.NOT.value + l1 in c2:
                    same_literals.extend([l1, str(Operator.NOT.value + l1)])
            elif len(l1) == 2:
                if l1[1] in c2:
                    same_literals.extend([l1[1], l1])
        clause_sum = c1 + c2
        resolvent = [lit for lit in clause_sum if lit not in same_literals]
        return list(set(resolvent))


    @staticmethod
    def check_for_tautology_in_set_of_literals(clauses):
        for index, clause in enumerate(clauses):
            literals = []
            neg_literals = []
            for i, literal in enumerate(clause):
                if Operator.NOT.value in literal:
                    neg_literals.append(literal[1])
                else:
                    literals.append(literal)
            tautology_literals = set(literals) & set(neg_literals)
            if len(tautology_literals) > 0:
                print(str(tautology_literals) + " is tautology, literal removed.")
                clauses[index].remove(list(tautology_literals)[0])
                clauses[index].remove("¬" + list(tautology_literals)[0])


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


def formula_to_str(formula):
    """Return formula in readable format"""
    out_formula = ""
    for clause in formula.clause:
        if isinstance(clause, Clause):
            out_formula += "(" + str(formula_to_str(clause)) + ")"
        else:
            out_formula += clause
    return out_formula
