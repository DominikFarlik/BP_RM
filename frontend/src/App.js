import React, {useState} from "react";
import {BrowserRouter as Router, Route, Routes, Navigate} from "react-router-dom";
import Login from "./Login";
import Register from "./Register";
import Solve from "./Solve";
import Navbar from "./Navbar";

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
                <Navbar token={token} handleLogout={handleLogout} />
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
