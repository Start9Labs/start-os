--
-- PostgreSQL database dump
--

-- Dumped from database version 14.12 (Ubuntu 14.12-0ubuntu0.22.04.1)
-- Dumped by pg_dump version 14.12 (Ubuntu 14.12-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: admin; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.admin (
    id character varying NOT NULL,
    created_at timestamp with time zone NOT NULL,
    pass_hash character varying NOT NULL,
    deleted_at timestamp with time zone
);


ALTER TABLE public.admin OWNER TO alpha_admin;

--
-- Name: admin_pkgs; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.admin_pkgs (
    id bigint NOT NULL,
    admin character varying NOT NULL,
    pkg_id character varying NOT NULL
);


ALTER TABLE public.admin_pkgs OWNER TO alpha_admin;

--
-- Name: admin_pkgs_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.admin_pkgs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.admin_pkgs_id_seq OWNER TO alpha_admin;

--
-- Name: admin_pkgs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.admin_pkgs_id_seq OWNED BY public.admin_pkgs.id;


--
-- Name: category; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.category (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    name character varying NOT NULL,
    description character varying NOT NULL,
    priority bigint DEFAULT 0 NOT NULL
);


ALTER TABLE public.category OWNER TO alpha_admin;

--
-- Name: category_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.category_id_seq OWNER TO alpha_admin;

--
-- Name: category_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.category_id_seq OWNED BY public.category.id;


--
-- Name: eos_hash; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.eos_hash (
    id bigint NOT NULL,
    version character varying NOT NULL,
    hash character varying NOT NULL
);


ALTER TABLE public.eos_hash OWNER TO alpha_admin;

--
-- Name: eos_hash_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.eos_hash_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.eos_hash_id_seq OWNER TO alpha_admin;

--
-- Name: eos_hash_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.eos_hash_id_seq OWNED BY public.eos_hash.id;


--
-- Name: error_log_record; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.error_log_record (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    epoch character varying NOT NULL,
    commit_hash character varying NOT NULL,
    source_file character varying NOT NULL,
    line bigint NOT NULL,
    target character varying NOT NULL,
    level character varying NOT NULL,
    message character varying NOT NULL,
    incidents bigint NOT NULL
);


ALTER TABLE public.error_log_record OWNER TO alpha_admin;

--
-- Name: error_log_record_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.error_log_record_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.error_log_record_id_seq OWNER TO alpha_admin;

--
-- Name: error_log_record_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.error_log_record_id_seq OWNED BY public.error_log_record.id;


--
-- Name: metric; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.metric (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    version character varying NOT NULL,
    pkg_id character varying NOT NULL
);


ALTER TABLE public.metric OWNER TO alpha_admin;

--
-- Name: metric_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.metric_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.metric_id_seq OWNER TO alpha_admin;

--
-- Name: metric_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.metric_id_seq OWNED BY public.metric.id;


--
-- Name: os_version; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.os_version (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    number character varying NOT NULL,
    headline character varying NOT NULL,
    release_notes character varying NOT NULL,
    arch character varying
);


ALTER TABLE public.os_version OWNER TO alpha_admin;

--
-- Name: os_version_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.os_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.os_version_id_seq OWNER TO alpha_admin;

--
-- Name: os_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.os_version_id_seq OWNED BY public.os_version.id;


--
-- Name: persistent_migration; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.persistent_migration (
    id integer NOT NULL,
    version integer NOT NULL,
    label character varying,
    "timestamp" timestamp with time zone NOT NULL
);


ALTER TABLE public.persistent_migration OWNER TO alpha_admin;

--
-- Name: persistent_migration_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.persistent_migration_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.persistent_migration_id_seq OWNER TO alpha_admin;

--
-- Name: persistent_migration_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.persistent_migration_id_seq OWNED BY public.persistent_migration.id;


--
-- Name: pkg_category; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.pkg_category (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    category_id bigint NOT NULL,
    pkg_id character varying NOT NULL
);


ALTER TABLE public.pkg_category OWNER TO alpha_admin;

--
-- Name: pkg_dependency; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.pkg_dependency (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    pkg_id character varying NOT NULL,
    pkg_version character varying NOT NULL,
    dep_id character varying NOT NULL,
    dep_version_range character varying NOT NULL
);


ALTER TABLE public.pkg_dependency OWNER TO alpha_admin;

--
-- Name: pkg_dependency_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.pkg_dependency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pkg_dependency_id_seq OWNER TO alpha_admin;

--
-- Name: pkg_dependency_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.pkg_dependency_id_seq OWNED BY public.pkg_dependency.id;


--
-- Name: pkg_record; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.pkg_record (
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone,
    pkg_id character varying NOT NULL,
    hidden boolean DEFAULT false NOT NULL
);


ALTER TABLE public.pkg_record OWNER TO alpha_admin;

--
-- Name: service_category_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.service_category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.service_category_id_seq OWNER TO alpha_admin;

--
-- Name: service_category_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.service_category_id_seq OWNED BY public.pkg_category.id;


--
-- Name: upload; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.upload (
    id bigint NOT NULL,
    uploader character varying NOT NULL,
    pkg_id character varying NOT NULL,
    pkg_version character varying NOT NULL,
    created_at timestamp with time zone NOT NULL
);


ALTER TABLE public.upload OWNER TO alpha_admin;

--
-- Name: upload_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.upload_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.upload_id_seq OWNER TO alpha_admin;

--
-- Name: upload_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.upload_id_seq OWNED BY public.upload.id;


--
-- Name: user_activity; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.user_activity (
    id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    server_id character varying NOT NULL,
    os_version character varying,
    arch character varying
);


ALTER TABLE public.user_activity OWNER TO alpha_admin;

--
-- Name: user_activity_id_seq; Type: SEQUENCE; Schema: public; Owner: alpha_admin
--

CREATE SEQUENCE public.user_activity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_activity_id_seq OWNER TO alpha_admin;

--
-- Name: user_activity_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: alpha_admin
--

ALTER SEQUENCE public.user_activity_id_seq OWNED BY public.user_activity.id;


--
-- Name: version; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.version (
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone,
    number character varying NOT NULL,
    release_notes character varying NOT NULL,
    os_version character varying NOT NULL,
    pkg_id character varying NOT NULL,
    title character varying NOT NULL,
    desc_short character varying NOT NULL,
    desc_long character varying NOT NULL,
    icon_type character varying NOT NULL,
    deprecated_at timestamp with time zone
);


ALTER TABLE public.version OWNER TO alpha_admin;

--
-- Name: version_platform; Type: TABLE; Schema: public; Owner: alpha_admin
--

CREATE TABLE public.version_platform (
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone,
    pkg_id character varying NOT NULL,
    version_number character varying NOT NULL,
    arch character varying NOT NULL,
    ram bigint,
    device jsonb
);


ALTER TABLE public.version_platform OWNER TO alpha_admin;

--
-- Name: admin_pkgs id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.admin_pkgs ALTER COLUMN id SET DEFAULT nextval('public.admin_pkgs_id_seq'::regclass);


--
-- Name: category id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.category ALTER COLUMN id SET DEFAULT nextval('public.category_id_seq'::regclass);


--
-- Name: eos_hash id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.eos_hash ALTER COLUMN id SET DEFAULT nextval('public.eos_hash_id_seq'::regclass);


--
-- Name: error_log_record id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.error_log_record ALTER COLUMN id SET DEFAULT nextval('public.error_log_record_id_seq'::regclass);


--
-- Name: metric id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.metric ALTER COLUMN id SET DEFAULT nextval('public.metric_id_seq'::regclass);


--
-- Name: os_version id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.os_version ALTER COLUMN id SET DEFAULT nextval('public.os_version_id_seq'::regclass);


--
-- Name: persistent_migration id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.persistent_migration ALTER COLUMN id SET DEFAULT nextval('public.persistent_migration_id_seq'::regclass);


--
-- Name: pkg_category id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_category ALTER COLUMN id SET DEFAULT nextval('public.service_category_id_seq'::regclass);


--
-- Name: pkg_dependency id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_dependency ALTER COLUMN id SET DEFAULT nextval('public.pkg_dependency_id_seq'::regclass);


--
-- Name: upload id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.upload ALTER COLUMN id SET DEFAULT nextval('public.upload_id_seq'::regclass);


--
-- Name: user_activity id; Type: DEFAULT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.user_activity ALTER COLUMN id SET DEFAULT nextval('public.user_activity_id_seq'::regclass);


--
-- Name: admin admin_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.admin
    ADD CONSTRAINT admin_pkey PRIMARY KEY (id);


--
-- Name: admin_pkgs admin_pkgs_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.admin_pkgs
    ADD CONSTRAINT admin_pkgs_pkey PRIMARY KEY (id);


--
-- Name: category category_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);


--
-- Name: eos_hash eos_hash_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.eos_hash
    ADD CONSTRAINT eos_hash_pkey PRIMARY KEY (id);


--
-- Name: error_log_record error_log_record_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.error_log_record
    ADD CONSTRAINT error_log_record_pkey PRIMARY KEY (id);


--
-- Name: metric metric_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.metric
    ADD CONSTRAINT metric_pkey PRIMARY KEY (id);


--
-- Name: os_version os_version_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.os_version
    ADD CONSTRAINT os_version_pkey PRIMARY KEY (id);


--
-- Name: persistent_migration persistent_migration_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.persistent_migration
    ADD CONSTRAINT persistent_migration_pkey PRIMARY KEY (id);


--
-- Name: pkg_category pkg_category_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_category
    ADD CONSTRAINT pkg_category_pkey PRIMARY KEY (id);


--
-- Name: pkg_dependency pkg_dependency_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_dependency
    ADD CONSTRAINT pkg_dependency_pkey PRIMARY KEY (id);


--
-- Name: admin_pkgs unique_admin_pkg; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.admin_pkgs
    ADD CONSTRAINT unique_admin_pkg UNIQUE (pkg_id, admin);


--
-- Name: error_log_record unique_log_record; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.error_log_record
    ADD CONSTRAINT unique_log_record UNIQUE (epoch, commit_hash, source_file, line, target, level, message);


--
-- Name: category unique_name; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.category
    ADD CONSTRAINT unique_name UNIQUE (name);


--
-- Name: pkg_category unique_pkg_category; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_category
    ADD CONSTRAINT unique_pkg_category UNIQUE (pkg_id, category_id);


--
-- Name: pkg_dependency unique_pkg_dep_version; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_dependency
    ADD CONSTRAINT unique_pkg_dep_version UNIQUE (pkg_id, pkg_version, dep_id);


--
-- Name: eos_hash unique_version; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.eos_hash
    ADD CONSTRAINT unique_version UNIQUE (version);


--
-- Name: upload upload_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.upload
    ADD CONSTRAINT upload_pkey PRIMARY KEY (id);


--
-- Name: user_activity user_activity_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.user_activity
    ADD CONSTRAINT user_activity_pkey PRIMARY KEY (id);


--
-- Name: version version_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.version
    ADD CONSTRAINT version_pkey PRIMARY KEY (pkg_id, number);


--
-- Name: version_platform version_platform_pkey; Type: CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.version_platform
    ADD CONSTRAINT version_platform_pkey PRIMARY KEY (pkg_id, version_number, arch);


--
-- Name: category_name_idx; Type: INDEX; Schema: public; Owner: alpha_admin
--

CREATE UNIQUE INDEX category_name_idx ON public.category USING btree (name);


--
-- Name: pkg_record_pkg_id_idx; Type: INDEX; Schema: public; Owner: alpha_admin
--

CREATE UNIQUE INDEX pkg_record_pkg_id_idx ON public.pkg_record USING btree (pkg_id);


--
-- Name: version_number_idx; Type: INDEX; Schema: public; Owner: alpha_admin
--

CREATE INDEX version_number_idx ON public.version USING btree (number);


--
-- Name: admin_pkgs admin_pkgs_admin_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.admin_pkgs
    ADD CONSTRAINT admin_pkgs_admin_fkey FOREIGN KEY (admin) REFERENCES public.admin(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: metric metric_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.metric
    ADD CONSTRAINT metric_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pkg_category pkg_category_category_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_category
    ADD CONSTRAINT pkg_category_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.category(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pkg_category pkg_category_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_category
    ADD CONSTRAINT pkg_category_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pkg_dependency pkg_dependency_dep_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_dependency
    ADD CONSTRAINT pkg_dependency_dep_id_fkey FOREIGN KEY (dep_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: pkg_dependency pkg_dependency_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.pkg_dependency
    ADD CONSTRAINT pkg_dependency_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: upload upload_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.upload
    ADD CONSTRAINT upload_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: upload upload_uploader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.upload
    ADD CONSTRAINT upload_uploader_fkey FOREIGN KEY (uploader) REFERENCES public.admin(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: version version_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.version
    ADD CONSTRAINT version_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: version_platform version_platform_pkg_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: alpha_admin
--

ALTER TABLE ONLY public.version_platform
    ADD CONSTRAINT version_platform_pkg_id_fkey FOREIGN KEY (pkg_id) REFERENCES public.pkg_record(pkg_id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- PostgreSQL database dump complete
--

