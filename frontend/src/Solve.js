import React, {useState, useEffect} from 'react';
import axios from 'axios';
import './App.css';
import {useNavigate} from 'react-router-dom';

const LogicFormulaApp = () => {
    const [formula, setFormula] = useState('');
    const [result, setResult] = useState(null);
    const [steps, setSteps] = useState([]);
    const [error, setError] = useState('');
    const navigate = useNavigate();

    useEffect(() => {
        const token = localStorage.getItem('token');
        if (!token) {
            navigate('/login');
        }
    }, [navigate]);

    const handleSubmit = async (e) => {
        e.preventDefault();
        setError('');
        setSteps([]);

        const token = localStorage.getItem('token');
        if (!token) {
            setError('You are not logged in. Please log in to solve formulas.');
            navigate('/login');
            return;
        }

        try {
            const response = await axios.post(
                'http://localhost:5000/api/solve',
                {formula},
                {
                    headers: {
                        Authorization: `Bearer ${token}`, // Include the JWT token
                    },
                }
            );
            setSteps(response.data.steps);
        } catch (err) {
            if (err.response && err.response.status === 401) {
                setError('Session expired. Please log in again.');
                localStorage.removeItem('token');
                navigate('/login');
            } else {
                setError('An error occurred while processing the formula. Please try again.');
            }
            console.error(err);
        }
    };

    return (
        <div className="container-sm shadow p-3 mb-5 bg-body-tertiary rounded" style={{width: "70%", marginTop: "2%"}}>
            <form onSubmit={handleSubmit}>
                <div className="input-group mb-3">
                    <input type="text" id="formula" value={formula} onChange={(e) => setFormula(e.target.value)}
                           className="form-control" placeholder="Enter formula"
                    />

                    <button type="submit" className="btn btn-success">Solve</button>
                </div>
            </form>

            {error && <p className="error">{error}</p>}

            {steps.length > 0 && (
                <div className="steps-container">
                    <h2 className="sub-header">Steps</h2>
                    <ol className="steps-list">
                        {steps.map((step) => (
                            <li className="step-item">{step}</li>
                        ))}
                    </ol>
                </div>
            )}
        </div>
    );
};

export default LogicFormulaApp;
