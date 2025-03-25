from sympy import symbols  # type: ignore
from sympy.logic.boolalg import to_cnf  # type: ignore
from sympy.logic.inference import satisfiable  # type: ignore
import re

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


def rewrite_equivalence(expression):
    pattern = r"(\w+)\s*↔\s*(\w+)"
    return re.sub(pattern, r"(\1 | ~\2) & (~\1 | \2)", expression)


def solve(formula: str):
    formated_formula = prepare_for_cnf(formula)
    found_symbols = init_symbols(formated_formula)
    expression = eval(formated_formula, found_symbols)
    cnf = to_cnf(expression)
    cnf_str = str(cnf)
    back_translated_formula = cnf_str.translate(REVERSE_OPERATOR_TRANSLATOR)
    clause_list = split_to_list_of_literals(back_translated_formula)
    steps = resolution(clause_list)
    return steps


def prepare_for_cnf(formula: str) -> str:
    translated_formula = formula.translate(OPERATOR_TRANSLATOR)
    formula_without_equivalence = rewrite_equivalence(translated_formula)
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
        steps = ["Množina klauzulí: %s" % clauses_to_string(clauses)]

    def update_clauses(clause1, clause2):
        resolvent = make_resolvent(clause1, clause2)
        steps.append("Z klauzulí: " + clauses_to_string([clause1]) + " a " + clauses_to_string([clause2]) + " vznikne resolventa: " + clauses_to_string([resolvent]))
        clauses.remove(clause1)
        clauses.remove(clause2)
        clauses.append(resolvent)
        steps.append("Zbývající klauzule: %s" % clauses_to_string(clauses))

    found = False
    while True:
        if found:
            found = False
        for clause in clauses:
            for literal in clause:
                for compared_clause in clauses:
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
            return steps


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
        clauses_str += ",".join(clause) + "},"
    return clauses_str[:-1]