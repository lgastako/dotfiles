DROP TABLE IF EXISTS notes;
DROP TABLE IF EXISTS package_platform;
DROP TABLE IF EXISTS installations;
DROP TABLE IF EXISTS machines_platforms;
DROP TABLE IF EXISTS platforms;
DROP TABLE IF EXISTS versions;
DROP TABLE IF EXISTS packages;

-- TODO: created_at triggers etc

CREATE TABLE packages (
    package_id INTEGER PRIMARY KEY, 
    description TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

INSERT INTO packages (package_id, description) VALUES (1, "emacs");
INSERT INTO packages (package_id, description) VALUES (2, "vim");
INSERT INTO packages (package_id, description) VALUES (3, "zsh");
INSERT INTO packages (package_id, description) VALUES (4, "bash");
INSERT INTO packages (package_id, description) VALUES (6, "hg");
INSERT INTO packages (package_id, description) VALUES (7, "x11");

CREATE TABLE package_groups (
    package_id INTEGER NOT NULL REFERENCES packages(package_id) ON DELETE CASCADE,
    name TEXT UNIQUE NOT NULL
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

INSERT INTO package_groups (package_group_id, name) VALUES (1, "dev");
INSERT INTO package_groups (package_group_id, name) VALUES (2, "games");
INSERT INTO package_groups (package_group_id, name) VALUES (3, "tunes");

CREATE TABLE versions (
    version_id INTEGER PRIMARY KEY,
    package_id INTEGER NOT NULL REFERENCES packages(package_id) ON DELETE CASCADE,
    version_description TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

INSERT INTO versions (version_id, package_id, version_description) VALUES (1, 1, "No time.  I'll have to hack on this when I have a chance.:wq :)");

CREATE TABLE platforms (
    platform_id INTEGER PRIMARY KEY,
    description TEXT UNIQUE NOT NULL
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

CREATE TABLE machines (
    machine_id INTEGER PRIMARY KEY,
    platform_id INTEGER NOT NULL,
    description TEXT UNIQUE NOT NULL
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

CREATE TABLE platform_groups (
    platform_id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

CREATE TABLE platform_notes (
    platform_id INTEGER PRIMARY KEY,
    note TEXT UNIQUE NOT NULL
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);

CREATE TABLE oneoff_package_installations (
    platform_id INTEGER NOT NULL REFERENCES platforms(platform_id) ON DELETE CASCADE,
    package_id INTEGER NOT NULL REFERENCES package_(package_id) ON DELETE CASCADE,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    PRIMARY KEY (platform_id, package_id)
);

CREATE TABLE package_notes (
    package_id INTEGER NOT NULL REFERENCES packages(package_id) ON DELETE CASCADE,
    platform_id INTEGER NOT NULL REFERENCES packages(package_id) ON DELETE CASCADE,
    note TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL    
    PRIMARY KEY (package_id, platform_id)
);

CREATE TABLE virtualenvs (
    virutal_env_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE oneoff_virtualenv_installations (
    platform_id INTEGER NOT NULL REFERENCES platforms(platform_id),
    virtualenv_id INTEGER NOT NULL REFERENCES virtualenvs(virtualenv_id),
    PRIMARY KEY (platform_id, virtualenv_id)
);

