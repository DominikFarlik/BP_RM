from Clause import parse_formula, print_formula

if __name__ == "__main__":
    input_formula = "(A∨B)∧(¬A∨¬C)∧(¬B∨¬C)∧(A∨C)"
    print("User input: " + " "*14 + input_formula)
    formula = parse_formula(input_formula)
    print("Parsed formula: " + " "*10 + print_formula(formula))
    formula.remove_implications()
    print("Removed implications: " + " "*4 + print_formula(formula))
    formula.remove_clause_negations()
    print("Removed clause negations: " + print_formula(formula))
    formula.distribute()
    print("Disjunction distribution: " + print_formula(formula))
    formula.resolute()
    print("Resolute: " + print_formula(formula))
