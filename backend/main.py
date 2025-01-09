from Clause import parse_formula, print_formula

if __name__ == "__main__":
    input_formula = "((A∨B)→C)∧(¬D∨(E→F))"
    print("User input: " + " "*14 + input_formula)
    formula = parse_formula(input_formula)
    print("Parsed formula: " + " "*10 + print_formula(formula))
    formula.remove_equivalences()
    print("Removed equivalences: " + " " * 4 + print_formula(formula))
    formula.remove_implications()
    print("Removed implications: " + " "*4 + print_formula(formula))
    formula.remove_clause_negations()
    print("Removed clause negations: " + print_formula(formula))
    formula.distribute()
    print("Disjunction distribution: " + print_formula(formula))
    formula.connect_clauses_with_same_operators()
    print("Removed unnecessary brackets: " + print_formula(formula))

    #formula.distribute()
    #formula.connect_clauses_with_same_operators()
    #print(" "*26 + print_formula(formula))

    #resolution = formula.resolute()
    #if not resolution:
    #    print("Empty clause left -> formula is not feasible.")
    #else:
    #    print("Final resolvent " + str(resolution) + " -> formula is feasible.")
