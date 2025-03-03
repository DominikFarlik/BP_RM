import copy
import sys

from Clause import parse_formula, formula_to_str
from flask import Flask, jsonify, request
from flask_cors import CORS
from flask_bcrypt import Bcrypt
from flask_sqlalchemy import SQLAlchemy
from flask_jwt_extended import JWTManager, create_access_token

app = Flask(__name__)
CORS(app)
bcrypt = Bcrypt(app)

app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///users.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['JWT_SECRET_KEY'] = 'jwt_secret_key'

db = SQLAlchemy(app)
jwt = JWTManager(app)

class User(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(80), unique=True, nullable=False)
    password = db.Column(db.String(200), nullable=False)
    
# Initialize database
with app.app_context():
    db.create_all()

def process_formula(input_formula):
    steps = []

    steps.append("User input: " + str(input_formula))
    formula = parse_formula(str(input_formula))
    print(formula_to_str(formula))
    formula.remove_equivalences()
    steps.append("Removed equivalences: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.remove_implications()
    steps.append("Removed implications: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.remove_clause_negations()
    steps.append("Removed clause negations: " + formula_to_str(formula))
    print(formula_to_str(formula))

    while True:
        formula_before_distribution = copy.deepcopy(formula)
        formula.distribute()
        formula.connect_clauses_with_same_operators()
        if formula_to_str(formula_before_distribution) == formula_to_str(formula):
            break

    steps.append("Distributed formula: " + formula_to_str(formula))
    print(formula_to_str(formula))
    formula.connect_clauses_with_same_operators()
    print(formula_to_str(formula))

    resolution, resolution_steps = formula.resolute()
    steps.extend(resolution_steps)

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

# API: Register user
@app.route('/api/register', methods=['POST'])
def register_user():
    try:
        data = request.get_json()
        username = data.get('username')
        password = data.get('password')

        if not username or not password:
            return jsonify({"error": "Username and password are required."}), 400

        if User.query.filter_by(username=username).first():
            return jsonify({"error": "Username already exists."}), 400

        hashed_password = bcrypt.generate_password_hash(password).decode('utf-8')
        new_user = User(username=username, password=hashed_password)
        db.session.add(new_user)
        db.session.commit()

        return jsonify({"message": "User registered successfully."}), 201
    except Exception as e:
        return jsonify({"error": str(e)}), 500

# API: Login user
@app.route('/api/login', methods=['POST'])
def login_user():
    try:
        data = request.get_json()
        username = data.get('username')
        password = data.get('password')

        if not username or not password:
            return jsonify({"error": "Username and password are required."}), 400

        user = User.query.filter_by(username=username).first()

        if not user or not bcrypt.check_password_hash(user.password, password):
            return jsonify({"error": "Invalid username or password."}), 401

        # Create JWT token
        access_token = create_access_token(identity={'username': user.username})
        return jsonify({
            "message": "Login successful.",
            "access_token": access_token
        }), 200
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == '__main__':
    print("\nP∨(Q∧R):")
    process_formula("P∨(Q∧R)")
    print("\nP→(Q→R):")
    process_formula("P→(Q→R)")
    print("\nP→(Q↔R):")
    process_formula("P→(Q↔R)")
    print("\nP∨(Q↔R):")
    process_formula("P∨(Q↔R)")
    #app.run(debug=True)
