import sympy.logic
from sympy import symbols
from sympy.logic.boolalg import to_cnf
from sympy.logic.inference import satisfiable
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


def convert_to_cnf(formula):
    translated_formula = formula.translate(OPERATOR_TRANSLATOR)
    translated_formula = rewrite_equivalence(translated_formula)
    variables = set(re.findall(r"[A-Za-z]", translated_formula))
    symbols_dict = {var: symbols(var) for var in variables}
    expr = eval(translated_formula, symbols_dict)
    formula_in_cnf = to_cnf(expr)
    print(satisfiable(formula_in_cnf))
    result_formula = str(formula_in_cnf)
    result_formula_back_translated = result_formula.translate(REVERSE_OPERATOR_TRANSLATOR)
    print(result_formula_back_translated)
