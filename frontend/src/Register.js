import React, { useState } from "react";
import { registerUser } from "./Api";
import AuthForm from "./AuthForm";

function Register() {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [message, setMessage] = useState("");
  const [error, setError] = useState("");

  const handleRegister = async (e) => {
    e.preventDefault();
    try {
      await registerUser(username, password);
      setMessage("Registration successful! You can now log in.");
      setUsername("");
      setPassword("");
    } catch (err) {
      setError(err.response?.data?.error || "Registration failed");
    }
  };

  return (
    <AuthForm type="register" onSubmit={handleRegister} />
  );
}

export default Register;
