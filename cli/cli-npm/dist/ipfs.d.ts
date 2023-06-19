type UploadResult = {
    cid: string;
    size: number;
};
export declare function uploadFile(path: string, multiaddrResult: any, infoLogger: (s: string) => void, errorLogger: (s: string) => void): Promise<UploadResult>;
export {};
