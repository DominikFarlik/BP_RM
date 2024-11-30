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
                new_clause.append(clause[i - 1][0][1])

            else:
                new_clause.append([Operator.NOT.value])
                new_clause.append(clause[i - 1])

            new_clause.append([Operator.OR.value])
            new_clause.append(clause[i + 1])

    return new_clause