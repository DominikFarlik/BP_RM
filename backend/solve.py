from sympy import symbols  # type: ignore
from sympy.logic.boolalg import to_cnf  # type: ignore
from sympy.logic.inference import satisfiable  # type: ignore
import re
import copy

OPERATOR_DICT = {
    "¬": "~",
    "∧": "&",
    "∨": "|",
    "→": ">>",
}

OPERATOR_TRANSLATOR = str.maketrans(OPERATOR_DICT)

REVERSE_OPERATOR_DICT = {
    "~": "¬",
    "&": "∧",
    "|": "∨",
}

REVERSE_OPERATOR_TRANSLATOR = str.maketrans(REVERSE_OPERATOR_DICT)


def get_bracket_index(start, stop, expr, br_type):
    br_count = 1
    br_index = 0
    first_br = "("
    second_br = ")"
    step = 1

    if br_type == "open":
        first_br = "("
        second_br = ")"
        step = -1
    elif br_type == "close":
        first_br = ")"
        second_br = "("
        step = 1

    for index in range(start, stop, step):
        if expr[index] == first_br:
            br_count -= 1
            if br_count < 1:
                br_index = index
                break
        elif expr[index] == second_br:
            br_count += 1
    return br_index

def rewrite_equivalence(expression):
    eq_count = expression.count("↔")
    for i in range(0, eq_count):
        for index, char in enumerate(expression):
            if char == "↔":
                print(expression)
                if expression[index - 1] == ")" and expression[index + 1] == "(":
                    first_br_start = get_bracket_index(index - 2, 0, expression, "open")
                    second_br_end = get_bracket_index(index + 2, len(expression), expression, "close")
                    first_br = "(" + "~" + expression[first_br_start:index] + "|" + expression[index + 1:second_br_end + 1] + ")"
                    second_br = "(" + expression[first_br_start:index] + "|" + "~" + expression[index + 1:second_br_end + 1] + ")"
                    expression = expression[0:first_br_start] + first_br + "&" + second_br + expression[second_br_end + 1:]
                    break

                if expression[index - 1] == ")" and expression[index + 1] != "(":
                    first_br_start = get_bracket_index(index - 2, 0, expression, "open")
                    first_br = "(" + "~" + expression[first_br_start:index] + "|" + expression[index + 1:] + ")"
                    second_br = "(" + expression[first_br_start:index] + "|" + "~" + expression[index + 1:] + ")"
                    expression = expression[0:first_br_start] + first_br + "&" + second_br + expression[index + 2:]
                    break

                if expression[index - 1] != ")" and expression[index + 1] == "(":
                    second_br_end = get_bracket_index(index + 2, len(expression), expression, "close")
                    first_br = "(" + "~" + expression[index - 1] + "|" + expression[index + 1:second_br_end + 1] + ")"
                    second_br = "(" + expression[index - 1] + "|" + "~" + expression[index + 1:second_br_end + 1] + ")"
                    expression = expression[0:index - 1] + first_br + "&" + second_br + expression[second_br_end + 1:]
                    break

                if expression[index - 1] != ")" and expression[index + 1] != "(":
                    first_br = "(" + "~" + expression[index - 1] + "|" + expression[index + 1] + ")"
                    second_br = "(" + expression[index - 1] + "|" + "~" + expression[index + 1] + ")"
                    expression = expression[0:index - 1] + first_br + "&" + second_br + expression[index + 2:]
                    break

    return expression


def solve(formula: str):
    steps = []
    formated_formula = prepare_for_cnf(formula)
    steps.append("Rozložení ekvivalencí: %s" % formated_formula.translate(REVERSE_OPERATOR_TRANSLATOR))
    found_symbols = init_symbols(formated_formula)
    expression = eval(formated_formula, found_symbols)
    cnf = to_cnf(expression)
    cnf_str = str(cnf)
    back_translated_formula = cnf_str.translate(REVERSE_OPERATOR_TRANSLATOR)
    steps.append("Převod do kunjuktivní normální formy: %s" % back_translated_formula)
    clause_list = split_to_list_of_literals(back_translated_formula)
    resolution_steps, result = resolution(clause_list)
    steps.extend(resolution_steps)
    return steps, result


def prepare_for_cnf(formula: str) -> str:
    translated_formula = formula.translate(OPERATOR_TRANSLATOR)
    print(translated_formula)
    formula_without_equivalence = rewrite_equivalence(translated_formula)
    print(formula_without_equivalence)
    return formula_without_equivalence


def init_symbols(formula: str) -> dict:
    variables = set(re.findall(r"[A-Za-z]", formula))
    return {var: symbols(var) for var in variables}


def split_to_list_of_literals(formula: str) -> list[list[str]]:
    clauses = []
    clauses_str = formula.split(' ∧ ')
    for clause in clauses_str:
        clause = clause.replace("(", "").replace(")", "")
        current_clause = clause.split(' ∨ ')
        clauses.append(current_clause)
    return clauses


def resolution(clauses):
    if clauses[0][0] == "True":
        return ["Formule je tautologie, není potřeba použít rezoluční metodu."]
    else:
        steps = ["Použití rezoluční metody:", "Množina klauzulí: %s" % clauses_to_string(clauses)]

    def update_clauses(clause1, clause2):
        resolvent = make_resolvent(copy.deepcopy(clause1), copy.deepcopy(clause2))
        steps.append("Z klauzulí: " + clauses_to_string([clause1]) + " a " + clauses_to_string([clause2]) + " vznikne resolventa: " + clauses_to_string([resolvent]))
        clauses.remove(clause1)
        clauses.remove(clause2)
        clauses.append(resolvent)
        steps.append("Zbývající klauzule: %s" % clauses_to_string(clauses))

    found = False
    while True:
        if found:
            found = False
        for i, clause in enumerate(clauses):
            for literal in clause:
                for j, compared_clause in enumerate(clauses):
                    if i == j:
                        break
                    for compared_literal in compared_clause:
                        if len(literal) == 2 and len(compared_literal) == 1:
                            if literal[1] == compared_literal:
                                update_clauses(clause, compared_clause)
                                found = True
                                break

                        elif len(literal) == 1 and len(compared_literal) == 2:
                            if literal == compared_literal[1]:
                                update_clauses(clause, compared_clause)
                                found = True
                                break
                    if found:
                        break
                if found:
                    break
            if found:
                break
        if not found:
            steps.pop()
            result = get_result(clauses)
            steps.append("Zbytek po použití rezoluční metody: %s" % clauses_to_string(result))
            if result:
                steps.append("Formule není splnitelná")
                return steps, False
            else:
                steps.append("Formule je splnitelná")
                return steps, True



def make_resolvent(clause: list, clause2: list) -> list:
    resolvent = set(clause + clause2)
    resolvent_copy = resolvent.copy()
    for literal in resolvent_copy:
        if literal.startswith("¬"):
            opposite = literal[1:]
        else:
            opposite = "¬" + literal
        if opposite in resolvent:
            resolvent.remove(literal)
            resolvent.remove(opposite)

    return list(resolvent)


def clauses_to_string(clauses: list[list[str]]) -> str:
    clauses_str = ""
    for clause in clauses:
        clauses_str += "{"
        clauses_str += ", ".join(clause) + "},"
    return clauses_str[:-1]


def get_result(clauses):
    for clause in clauses:
        if not clause:
            clauses.remove(clause)
    print(clauses)
    return clauses