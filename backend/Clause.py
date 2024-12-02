from main import Operator

class Clause:
    def __init__(self, parent=None):
        self.clause = []
        self.parent =  self if parent is None else parent

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
        for clause in self.clause:
            if isinstance(clause, Clause):
                clause.remove_implication()
            elif clause == Operator.IMPLICATION.value:
                print(self.clause)