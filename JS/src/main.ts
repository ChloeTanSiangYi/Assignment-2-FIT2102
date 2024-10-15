import { fromEvent, merge, timer } from "rxjs";
import { map, mergeScan, first, switchMap } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

// Grab HTML elements
const markdownInput = document.getElementById("markdown-input") as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const titleInput = document.getElementById("title-input") as HTMLInputElement;
const saveButton = document.getElementById("save-button") as HTMLButtonElement;
const darkModeToggle = document.getElementById("dark-mode-toggle") as HTMLInputElement;

// Ensure to reference header and section elements
const header = document.querySelector("header") as HTMLElement;
const sections = document.querySelectorAll("section") as NodeListOf<HTMLElement>;
const htmlOutput = document.getElementById("html-output") as HTMLElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

const title$: Observable<Action> = fromEvent(titleInput, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, title: value }))
);

const save$: Observable<Action> = fromEvent(saveButton, "click").pipe(
    map(() => (s) => ({ ...s, save: true }))
);

const darkMode$: Observable<Action> = fromEvent(darkModeToggle, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((isDarkMode) => (s) => ({ ...s, darkMode: isDarkMode }))
);

// Function to calculate word and character counts
const countMetrics$ = fromEvent(markdownInput, "input").pipe(
    map(() => markdownInput.value),
    map((markdown) => ({
        wordCount: markdown.split(/\s+/).filter(Boolean).length,
        charCount: markdown.length,
    }))
);

// Subscribe to update word and character counts in UI
countMetrics$.subscribe(({ wordCount, charCount }) => {
    document.getElementById("word-count")!.textContent = `Word Count: ${wordCount}`;
    document.getElementById("char-count")!.textContent = `Character Count: ${charCount}`;
});

function getHTML(s: State): Observable<State> {
    if (s.save) {
        return ajax<{ success: boolean }>({
            url: "/api/saveHTML",
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                html: s.HTML,
                title: s.title,
                // timestamp: getTime(), -- fix later
            }),
        }).pipe(
            map(() => {
                return { ...s, save: false }; // Reset the save flag
            }),
            first(),
        );
    }

    // Normal HTML conversion flow
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response),
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: "Converted HTML",
    darkMode: false,
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, title$, save$, darkMode$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(reducer)(resetState);
                return newReducer;
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
            // Update the page title
            document.title = value.title || "Converted HTML";

            // handle dark mode
            if (value.darkMode) {
                document.body.classList.add("dark-mode");
                header.classList.add("dark-mode");
                sections.forEach(section => section.classList.add("dark-mode"));
                htmlOutput.classList.add("dark-mode");
            } else {
                document.body.classList.remove("dark-mode");
                header.classList.remove("dark-mode");
                sections.forEach(section => section.classList.remove("dark-mode"));
                htmlOutput.classList.remove("dark-mode");
            }
        });
}

if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}