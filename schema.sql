CREATE TABLE tasks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    description TEXT NOT NULL,
    is_complete BOOLEAN NOT NULL,
    priority TEXT NOT NULL,
    due_date TEXT,
    category TEXT NOT NULL
);