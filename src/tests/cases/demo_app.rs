use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Platform {
            fn create_window(&self) -> Rc<dyn Window>;
        }

        pub trait Window {
            fn get_title(&self) -> String;
            fn set_title(&self, title: &str);

            fn get_size(&self) -> (i32, i32);
            fn set_size(&self, width: i32, height: i32);

            fn set_handler(&self, handler: Box<dyn Handler>);
            fn child_window(&self) -> Rc<dyn Window>;
        }

        pub trait Handler {
            fn on_draw(&self, canvas: Box<dyn Canvas>);
        }

        pub trait Canvas {
            fn draw_text_runs(&self, runs: Vec<TextRun>);
        }

        pub struct TextRun {
            pub text: String,
            pub rect: TextRect,
        }

        pub struct TextRect {
            pub x: i32,
            pub y: i32,
            pub w: i32,
            pub h: i32,
        }

        pub fn create_app(platform: Box<dyn Platform>) {
            let window = platform.create_window();

            assert_eq!(window.get_title(), "");
            assert_eq!(window.get_size(), (0, 0));

            window.set_title("Untitled");
            window.set_size(800, 600);

            assert_eq!(window.get_title(), "Untitled");
            assert_eq!(window.get_size(), (800, 600));

            let app = App {
                window: Rc::clone(&window),
            };
            window.set_handler(Box::new(app));
        }

        struct App {
            window: Rc<dyn Window>,
        }

        impl Handler for App {
            fn on_draw(&self, canvas: Box<dyn Canvas>) {
                let (w, _) = self.window.get_size();
                canvas.draw_text_runs(vec![
                    TextRun {
                        text: "ABC".into(),
                        rect: TextRect { x: 10, y: 10, w: w - 20, h: 20 },
                    },
                    TextRun {
                        text: "xyz".into(),
                        rect: TextRect { x: 10, y: 30, w: w - 20, h: 20 },
                    },
                ]);
            }
        }
    "#;

    case
}

test_all!();
