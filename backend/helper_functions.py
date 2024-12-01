from main import Operator

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
                    pass # TODO
                formula[i] = [new_clause]