@echo off
start cmd /k "cd backend && .\.venv\Scripts\python.exe main.py"
start cmd /k "cd frontend && npm start"
