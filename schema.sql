
CREATE TABLE password_recovery_token (
  profile_id uuid NOT NULL REFERENCES profile(id) ON DELETE CASCADE,
  token text NOT NULL,

  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  used_at timestamptz NULL,

  PRIMARY KEY (profile_id, token)
);



CREATE TABLE session (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),

  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  modified_at timestamptz NOT NULL DEFAULT clock_timestamp(),

  profile_id uuid REFERENCES profile(id) ON DELETE CASCADE,
  user_agent text NULL
);

CREATE INDEX session__profile_id__idx
    ON session(profile_id);

CREATE UNIQUE INDEX profile_email__email__idx
    ON profile_email (email);

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

COMMENT ON TABLE generic_token IS 'Table for generic tokens storage';

CREATE FUNCTION add(integer, integer) RETURNS integer
    AS 'select $1 + $2;'
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT;

