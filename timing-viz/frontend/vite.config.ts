import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
    plugins: [react()],
    server: {
        port: 5173,
        // Proxy keeps the wire URL relative (`/ws`, `/frame`) so production deployment is the
        // same surface as dev. The backend listens on 8765.
        proxy: {
            "/ws": {
                target: "ws://localhost:8765",
                ws: true,
                changeOrigin: true,
            },
            "/frame": "http://localhost:8765",
            "/replay": "http://localhost:8765",
        },
    },
});
