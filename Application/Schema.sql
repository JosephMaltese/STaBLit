-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    author TEXT NOT NULL,
    likecount INT DEFAULT 0 NOT NULL,
    dislikecount INT DEFAULT 0 NOT NULL,
    likes UUID[] DEFAULT '{}' NOT NULL,
    dislikes UUID[] DEFAULT '{}' NOT NULL
);
CREATE TABLE comments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    post_id UUID NOT NULL,
    author TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    parentid UUID DEFAULT NULL
);
CREATE INDEX comments_post_id_index ON comments (post_id);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts SMALLINT DEFAULT 0 NOT NULL,
    username TEXT DEFAULT '' NOT NULL
);
CREATE TABLE reactions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    postid UUID NOT NULL,
    userid UUID NOT NULL,
    emoji TEXT DEFAULT '' NOT NULL
);
ALTER TABLE comments ADD CONSTRAINT comments_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE NO ACTION;
