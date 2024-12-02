from Clause import parse_formula, print_formula

if __name__ == "__main__":
    input_formula = "((A→B)∨(¬C→(D∨¬E)))→(¬(A∨¬B)∨(F∧(G→H)))"
    print("User input: " + input_formula)
    formula = parse_formula(input_formula)
    print("Parsed formula: " + print_formula(formula))
    formula.remove_implications()
    print("Removed implications: " + print_formula(formula))
    formula.remove_clause_negations()
    print("Removed clause negations: " + print_formula(formula))
    formula.distribute()
    print("Disjunction distribution: " + print_formula(formula))
