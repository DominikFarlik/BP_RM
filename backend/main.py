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
    formula.distribute_disjunctions()
    print("Disjunction distribution: " + print_formula(formula))

    #formula1 = Formula(input_formula)
    #print("First split cycle:\n" + str(formula1) + "\n")
    #formula1.split_to_clauses_2nd_cycle()
    #print("Second split cycle:\n" + str(formula1) + "\n")
    #formula1.remove_implications_and_equivalences()  # TODO: finish  func
    #print("Removed implications:\n" + str(formula1) + "\n")
    #formula1.convert_to_NNF()  # TODO: finish  func
    #print("Converted to NNF:\n" + str(formula1) + "\n")
    #formula1.disjunction_distribution()  # TODO: finish  func
    #print("Disjunction distribution:\n" + str(formula1) + "\n")
    #if formula1.is_formula_in_cnf():  # TODO: finish  func
    #    formula1.negate_formula_for_resolution()  # TODO: finish  func
    #    # print("Negate each clause for resolution:\n" + str(formula1) + "\n")
    #    print("Resolution")
    #    formula1.resolute_formula()  # TODO: finish  func
