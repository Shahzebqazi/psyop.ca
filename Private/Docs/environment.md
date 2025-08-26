# Environment Files Overview

This project includes two example environment files. Use them to create your own `.env` file (not committed) for local or production use.

## Files

- `env.example`: Demonstrates typical development values and structure. Safe to read and copy from.
- `env.template`: A minimal, comment-annotated template to copy as a starting point. Values are blank or placeholders.

## Usage

1. Copy either file to `.env` in the project root.
2. Adjust values for your environment.
3. Do not commit `.env`.

```bash
# Example
cp env.template .env
# or
cp env.example .env
```

## Notes on Variables

- Server: `SERVER_HOST`, `SERVER_PORT`
- Environment: `ENVIRONMENT` (development|production)
- Assets: `ASSET_CDN_URL`, `ASSET_LOCAL_PATH`
- Logging: `LOG_LEVEL`, `LOG_FILE`
- Security: `SESSION_SECRET`, `CORS_ORIGINS`
- Dev: `HOT_RELOAD_ENABLED`, `DEBUG_MODE`

For production, ensure strong `SESSION_SECRET`, appropriate `CORS_ORIGINS`, and disable debug-style flags.

