import React, {useState} from "react";
import {BrowserRouter as Router, Route, Routes, Navigate, Link} from "react-router-dom";
import Login from "./Login";
import Register from "./Register";
import Solve from "./Solve";

function App() {
    const [token, setToken] = useState(localStorage.getItem("token"));

    const handleLogout = () => {
        localStorage.removeItem("token");
        setToken(null);
    };

    return (
        <Router>
            <div className="App">
                {/* Navigation Bar */}
                <nav className="navbar bg-body-tertiary">
                    <div className="container-fluid">
                            {!token ? (
                                <>
                                    <Link to="/login" className="btn btn-outline-success me-2">Login</Link>
                                    <Link to="/register" className="btn btn-outline-info me-2">Register</Link>
                                </>
                            ) : (
                                <>
                                    <Link to="/solve" className="btn btn-outline-info me-2">Solver</Link>
                                </>
                            )}

                            {token && (
                                <button className="btn btn-outline-danger" onClick={handleLogout}>
                                    Logout
                                </button>
                            )}
                    </div>
                </nav>

                    {/* Routes */
                    }
                    <Routes>
                        <Route path="/login" element={<Login setToken={setToken}/>}/>
                        <Route path="/register" element={<Register/>}/>
                        <Route path="/solve" element={token ? <Solve token={token}/> : <Navigate to="/login"/>}/>
                        <Route path="/" element={token ? <Navigate to="/solve"/> : <Navigate to="/login"/>}/>
                    </Routes>
            </div>
        </Router>
);
}

export default App;
