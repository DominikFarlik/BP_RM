import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { registerUser } from "./Api";
import AuthForm from "./AuthForm";

function Register() {
  const [error, setError] = useState("");
  const navigate = useNavigate();

  const handleRegister = async ({ username, password }) => {
    try {
      await registerUser(username, password);
      setError("");
      navigate("/login", { state: { message: "Registration successful! You can now log in." } });
    } catch (err) {
      setError(err.response?.data?.error || "Registration failed");
    }
  };

  return (
    <AuthForm type="register" onSubmit={handleRegister} error={error}/>
  );
}

export default Register;
