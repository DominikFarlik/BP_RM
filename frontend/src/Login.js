import React, { useState } from "react";
import { useNavigate } from "react-router-dom"; // Import useNavigate
import { loginUser } from "./Api";
import AuthForm from "./AuthForm";

function Login({ setToken }) {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const navigate = useNavigate(); // Initialize navigate

  const handleLogin = async (e) => {
    e.preventDefault();
    try {
      const response = await loginUser(username, password);
      const token = response.data.access_token;
      setToken(token);
      localStorage.setItem("token", token);
      navigate("/solve");
    } catch (err) {
      setError(err.response?.data?.error || "Login failed");
    }
  };

  return (
    <AuthForm type="login" onSubmit={handleLogin} />
  );
}

export default Login;
