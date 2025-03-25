import {Link, useLocation} from "react-router-dom";


const Navbar = ({ token, handleLogout }) => {
  const { pathname } = useLocation();

  return (
    <nav className="navbar bg-body-tertiary">
      <div className="container-fluid">
          <h4 className="text-success fw-bold">Logical formula solver</h4>
        {!token ? (
          pathname === "/login" ? (
            <Link to="/register" className="btn btn-outline-success me-2">Register</Link>
          ) : pathname === "/register" ? (
            <Link to="/login" className="btn btn-outline-success me-2">Login</Link>
          ) : null
        ) : (
          <>
            <button className="btn btn-outline-danger" onClick={handleLogout}>
              Logout
            </button>
          </>
        )}
      </div>
    </nav>
  );
};

export default Navbar;