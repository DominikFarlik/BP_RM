import React, { useState } from 'react';
import axios from 'axios';
import './App.css';

const LogicFormulaApp = () => {
  const [formula, setFormula] = useState('');
  const [result, setResult] = useState(null);
  const [steps, setSteps] = useState([]);
  const [error, setError] = useState('');

  const handleSubmit = async (e) => {
    e.preventDefault();
    setError('');
    setResult(null);
    setSteps([]);

    try {
      const response = await axios.post('http://localhost:5000/api/solve', { formula });
      setResult(response.data.result);
      setSteps(response.data.steps);
    } catch (err) {
      setError('An error occurred while processing the formula. Please try again.');
      console.error(err);
    }
  };

  return (
      <div className="container">
        <h1 className="header">Propositional Logic Solver</h1>
        <form onSubmit={handleSubmit} className="form">
          <label htmlFor="formula" className="label">Enter formula:</label>
          <input
              type="text"
              id="formula"
              value={formula}
              onChange={(e) => setFormula(e.target.value)}
              className="input"
              placeholder="Enter a logical formula..."
          />
          <button type="submit" className="button">Solve</button>
        </form>

        {error && <p className="error">{error}</p>}

        {result && (
            <div className="result-container">
              <h2 className="sub-header">Result</h2>
              <p className="result-text">{result}</p>
            </div>
        )}

        {steps.length > 0 && (
            <div className="steps-container">
              <h2 className="sub-header">Steps</h2>
              <ol className="steps-list">
                {steps.map((step, index) => (
                    <li key={index} className="step-item">{step}</li>
                ))}
              </ol>
            </div>
        )}
      </div>
  );
};


export default LogicFormulaApp;