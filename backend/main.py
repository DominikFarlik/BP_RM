from Clause import parse_formula, print_formula
from flask import Flask, jsonify, request
from flask_cors import CORS
from sympy import sympify
app = Flask(__name__)
CORS(app)

def process_formula(input_formula):
    steps = []
    x = "(A↔B)∧(¬A∨C)∧(C↔D)∧(¬D∨E)∧(B∨¬E)"

    steps.append("User input: " + str(input_formula))
    formula = parse_formula(str(input_formula))
    formula.remove_equivalences()
    steps.append("Removed equivalences: " + print_formula(formula))

    formula.remove_implications()
    steps.append("Removed implications: " + print_formula(formula))

    formula.remove_clause_negations()
    steps.append("Removed clause negations: " + print_formula(formula))

    formula.distribute()
    steps.append("Distributed formula: " + print_formula(formula))

    formula.connect_clauses_with_same_operators()
    formula.connect_clauses_with_same_operators()
    #print("Removed unnecessary brackets: " + print_formula(formula))

    formula.check_for_tautology_in_disjunctions()
    #print("After check for tautology: " + print_formula(formula))

    resolution = formula.resolute()
    if not resolution:
        print("Empty clause left -> formula is not feasible.")
        result = "Empty clause left -> formula is not feasible."
        return result, steps
    else:
        print("Final resolvent " + str(set(resolution[0])) + " -> formula is feasible.")
        result = "Final resolvent " + str(set(resolution[0])) + " -> formula is feasible."
        return result, steps

@app.route('/api/solve', methods=['POST'])
def solve_formula():
    try:
        data = request.get_json()
        formula = data.get('formula')

        if not formula:
            return jsonify({"error": "No formula provided."}), 400
        result, steps = process_formula(formula)
        print(result, steps)
        return jsonify({
            "result": result,
            "steps": steps
        })

    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == '__main__':
    app.run(debug=True)
