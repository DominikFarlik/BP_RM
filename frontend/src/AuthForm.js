import {useState} from "react";

const AuthForm = ({type, onSubmit}) => {
    const [username, setUsername] = useState("");
    const [password, setPassword] = useState("");

    const handleSubmit = (e) => {
        e.preventDefault();
        onSubmit({username, password});
    };

    return (
        <div className="container-sm" width="50%">
            <form onSubmit={handleSubmit}>
                <div className="mb-3">
                    <label className="form-label">Username</label>
                    <input type="text" value={username} onChange={(e) => setUsername(e.target.value)}
                           className="form-control"
                           required/>
                </div>
                <div className="mb-3">
                    <label className="form-label">Password</label>
                    <input
                        type="password" value={password} onChange={(e) => setPassword(e.target.value)}
                        className="form-control" required/>
                </div>
                <button type="submit" className="btn btn-primary">
                    {type === "login" ? "Login" : "Register"}
                </button>
            </form>
        </div>
    );
};

export default AuthForm;