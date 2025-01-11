import React, { useState } from 'react';
import axios from 'axios';

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
    <div style={{ padding: '20px', fontFamily: 'Arial, sans-serif' }}>
      <h1>Propositional Logic Solver</h1>
      <form onSubmit={handleSubmit} style={{ marginBottom: '20px' }}>
        <label htmlFor="formula" style={{ marginRight: '10px' }}>Enter formula:</label>
        <input
          type="text"
          id="formula"
          value={formula}
          onChange={(e) => setFormula(e.target.value)}
          style={{ marginRight: '10px', padding: '5px', width: '300px' }}
        />
        <button type="submit" style={{ padding: '5px 10px' }}>Submit</button>
      </form>

      {error && <p style={{ color: 'red' }}>{error}</p>}

      {result && (
        <div>
          <h2>Result</h2>
          <p>{result}</p>
        </div>
      )}

      {steps.length > 0 && (
        <div>
          <h2>Steps</h2>
          <ol>
            {steps.map((step, index) => (
              <li key={index}>{step}</li>
            ))}
          </ol>
        </div>
      )}
    </div>
  );
};

export default LogicFormulaApp;