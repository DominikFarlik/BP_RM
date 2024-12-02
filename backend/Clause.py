class Clause:
    def __init__(self, parent=None):
        self.clause = []
        self.parent =  self if parent is None else parent

    def __str__(self):
        clause = ""
        for item in self.clause:
            clause += str(item)
        return "(" + "".join(clause) + ")"

    def add_clause(self, parent):
        self.clause.append(Clause(parent))
        return self.clause[-1]

    def add_literal(self, literal):
        self.clause.append(literal)

    def get_parent(self):
        return self.parent