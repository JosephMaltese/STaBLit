CREATE TABLE reactions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    postid UUID NOT NULL,
    userid UUID NOT NULL,
    emoji TEXT DEFAULT '' NOT NULL
);
