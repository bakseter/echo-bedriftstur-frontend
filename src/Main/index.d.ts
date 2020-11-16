export namespace Elm {
    namespace Main {
        export interface App {
            ports: {
                sendSignInLink: {
                    subscribe(callback: (data: any) => void): void
                }
                getUserInfo: {
                    subscribe(callback: (data: any) => void): void
                }
                updateUserInfo: {
                    subscribe(callback: (data: any) => void): void
                }
                createTicket: {
                    subscribe(callback: (data: any) => void): void
                }
                signOut: {
                    subscribe(callback: (data: any) => void): void
                }
                userStatusChanged: {
                    send(data: any): void
                }
                sendSignInLinkSucceeded: {
                    send(data: any): void
                }
                sendSignInLinkError: {
                    send(data: any): void
                }
                signInSucceeded: {
                    send(data: any): void
                }
                signInError: {
                    send(data: any): void
                }
                getUserInfoSucceeded: {
                    send(data: any): void
                }
                getUserInfoError: {
                    send(data: any): void
                }
                updateUserInfoSucceeded: {
                    send(data: any): void
                }
                updateUserInfoError: {
                    send(data: any): void
                }
                createTicketSucceeded: {
                    send(data: any): void
                }
                createTicketError: {
                    send(data: any): void
                }
                signOutSucceeded: {
                    send(data: any): void
                }
                signOutError: {
                    send(data: any): void
                }
                lol: {
                    send(data: any): void
                }
            };
        }
        export function init(options: {
            node?: HTMLElement | null;
            flags: string;
        }): Elm.Main.App;
    }
}
