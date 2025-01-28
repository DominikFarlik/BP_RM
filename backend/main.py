import sys

from Clause import parse_formula, print_formula
from flask import Flask, jsonify, request
from flask_cors import CORS
from sympy import sympify
app = Flask(__name__)
CORS(app)

def process_formula(input_formula):
    steps = []

    steps.append("User input: " + str(input_formula))
    formula = parse_formula(str(input_formula))
    print(print_formula(formula))
    formula.remove_equivalences()
    steps.append("Removed equivalences: " + print_formula(formula))
    print(print_formula(formula))
    formula.remove_implications()
    steps.append("Removed implications: " + print_formula(formula))
    print(print_formula(formula))
    formula.remove_clause_negations()
    steps.append("Removed clause negations: " + print_formula(formula))
    print(print_formula(formula))
    formula.distribute()
    formula.connect_clauses_with_same_operators()
    formula.connect_clauses_with_same_operators()
    formula.distribute()
    steps.append("Distributed formula: " + print_formula(formula))
    print(print_formula(formula))
    formula.connect_clauses_with_same_operators()
    formula.connect_clauses_with_same_operators()
    print(print_formula(formula))
    formula.check_for_tautology_in_disjunctions()
    print(print_formula(formula))
    resolution, resolution_steps = formula.resolute()
    steps.extend(resolution_steps)
#(¬D ∨ E ∨ ¬A ∨ ¬B ∨ ¬C) ∧ (¬E ∨ D ∨ ¬A ∨ ¬B ∨ ¬C)
#(¬E ∨ D ∨ ¬C ∨ ¬A ∨ ¬B) ∧ (¬D ∨ E ∨ ¬C ∨ ¬A ∨ ¬B)
    if not resolution:
        print("Empty clause left -> formula is not feasible.")
        result = "Empty clause left -> formula is not feasible."
        return result, steps
    else:
        print("Final resolvent " + str(resolution) + " -> formula is feasible.")
        result = "Final resolvent " + str(resolution) + " -> formula is feasible."
        return result, steps

@app.route('/api/solve', methods=['POST'])
def solve_formula():
    try:
        data = request.get_json()
        formula = data.get('formula')
        if not formula:
            return jsonify({"error": "No formula provided."}), 400
        result, steps = process_formula(formula)
        return jsonify({
            "result": result,
            "steps": steps
        })

    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == '__main__':
    #process_formula("P∨(Q∧R)")
    #process_formula("P→(Q→R)")
    process_formula("P→(Q↔R)")
    #process_formula("P∨(Q↔R)")
    app.run(debug=True)
